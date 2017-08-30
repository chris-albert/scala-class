package reductions

import scala.annotation._
import org.scalameter._
import common._
import scala.collection.mutable

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
    val s = mutable.Stack[Char]()
    var balanced = true
    for(char <- chars) {
      if(char == '(') {
        s.push(char)
      } else if (char == ')') {
        if(s.isEmpty) {
          balanced = false
        } else {
          s.pop()
        }

      }
    }
    balanced && s.isEmpty
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): Int = {
      var i = idx
      var out = 0
      while(i < until && out >= 0) {
        val c = chars(i)
        if(c == '(') {
          out += 1
        } else if(c == ')') {
          out += out - 1
        }
        i += 1
      }
      out
    }

    def reduce(from: Int, until: Int): Int = {
      if(until - from <= threshold) {
        traverse(from,until,0,0)
      } else {
        ???
      }
    }

    reduce(0, chars.length) == 0
  }

  def parBalance2(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      ???
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
