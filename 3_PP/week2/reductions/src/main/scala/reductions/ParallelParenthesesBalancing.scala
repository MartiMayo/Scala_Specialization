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
    var counter = 0
    for (i <- 0 until chars.length) {
      if (chars(i) == '(') counter += 1
      else if(chars(i) == ')') counter -= 1
      if (counter < 0) return false
    }
    counter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var i = idx
      var x1 = 0
      var x1_acc = 0
      while (i < until) {
        if (chars(i) == '(') {
          if (x1_acc > 0) {
            x1 += x1_acc
            x1_acc = -1
          }
          else x1_acc -= 1

        }
        else if (chars(i) == ')') x1_acc += 1
        i += 1
      }
      if (x1_acc > 0) (x1 + x1_acc, 0)
      else (x1, -x1_acc)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold) traverse(from, until)
      else {
        val m = (from + until) / 2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, m), reduce(m, until))
        if (a2 - b1 > 0) (a1, a2 - b1 + b2)
        else (a1 + b1 - a2, b2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
