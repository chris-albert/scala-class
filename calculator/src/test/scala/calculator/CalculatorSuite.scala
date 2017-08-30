package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("Polynomial: compute delta") {
    val delta = Polynomial.computeDelta(Var(1),Var(1),Var(1))
    assert(delta() == -3)

    val d = Polynomial.computeDelta(Var(4),Var(10),Var(1))
    assert(d() == 84)
  }

  test("Polynomial: compute solutions") {
    val s = Polynomial.computeSolutions(Var(1),Var(1),Var(1),Var(-1))
    assert(s() == Set())

    val s2 = Polynomial.computeSolutions(Var(1),Var(-2),Var(-3),Var(16))
    assert(s2() == Set(3,-1))
  }

  test("Calculator: Self Dependency") {
    val values = Calculator.computeValues(Map(
      "a" -> Var(Literal(1))
    ))

    assert(values.get("a").map(_()).get == 1)

    //Easy cyclical
    val valuesC = Calculator.computeValues(Map(
      "a" -> Var(Ref("b")),
      "b" -> Var(Ref("a"))
    ))

    assert(valuesC.get("a").map(_()).get.equals(Double.NaN))
    assert(valuesC.get("b").map(_()).get.equals(Double.NaN))

    //Complex cyclical
    val valuesC2 = Calculator.computeValues(Map(
      "a" -> Var(Ref("f")),
      "b" -> Var(Ref("a")),
      "c" -> Var(Ref("b")),
      "d" -> Var(Ref("c")),
      "e" -> Var(Ref("d")),
      "f" -> Var(Ref("e"))
    ))

    assert(valuesC2.get("a").map(_()).get.equals(Double.NaN))
    assert(valuesC2.get("b").map(_()).get.equals(Double.NaN))
    assert(valuesC2.get("c").map(_()).get.equals(Double.NaN))
    assert(valuesC2.get("d").map(_()).get.equals(Double.NaN))
    assert(valuesC2.get("e").map(_()).get.equals(Double.NaN))
    assert(valuesC2.get("f").map(_()).get.equals(Double.NaN))

    //cyclical with math
    val valuesMath = Calculator.computeValues(Map(
      "a" -> Var(Plus(Ref("b"),Literal(1))),
      "b" -> Var(Times(Literal(2),Ref("a")))
    ))

    assert(valuesMath.get("a").map(_()).get.equals(Double.NaN))
    assert(valuesMath.get("b").map(_()).get.equals(Double.NaN))
  }
}
