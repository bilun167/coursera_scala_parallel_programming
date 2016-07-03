package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance0(i: Int, c: Int): Boolean = {
      if (i == chars.length) c == 0
      else if (chars(i) == ')' && c == 0) false
      else {
        val add = chars(i) match {
          case ')' => -1
          case '(' => 1
          case _ => 0
        }
        balance0(i + 1, c + add)
      }
    }

    balance0(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, c: Int, first: Int): (Int, Int) = {
      if (idx >= until) (c, first)
      else {
        val (c1, first1) = chars(idx) match {
          case '(' => (c + 1, if (first == 0) 1 else first)
          case ')' => (c - 1, if (first == 0) - 1 else first)
          case _ => (c, first)
        }
        traverse(idx + 1, until, c1, first1)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until <= from || until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (until - from) / 2
        val ((c1, first1), (c2, first2)) = parallel(reduce(from, mid), reduce(mid, until))
        (c1 + c2, if (first1 == 0) first2 else first1)
      }
    }

    val (c, f) = reduce(0, chars.length)
    c == 0 && f >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
