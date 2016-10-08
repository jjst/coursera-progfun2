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

  test("literal") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Literal(1) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 1
  }

  test("plus") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Plus(Literal(1),Literal(1)) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 2
  }

  test("minus") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Minus(Literal(1),Literal(1)) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 0
  }

  test("times") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Times(Literal(2),Literal(4)) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 8
  }

  test("divide") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Divide(Literal(4),Literal(2)) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 2
  }

  test("existing reference") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Literal(1) },
      "b" -> Signal { Ref("a") }
    )
    val res = Calculator.computeValues(expressions)
    res("a")() shouldBe 1
    res("b")() shouldBe 1
  }

  test("non-existing reference") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Ref("z") }
    )
    val res = Calculator.computeValues(expressions)
    res("a")().isNaN shouldBe true
  }

  test("cyclic reference") {
    val expressions = Map[String, Signal[Expr]](
      "a" -> Signal { Plus(Ref("a"), Literal(1)) }
    )
    val res = Calculator.computeValues(expressions)
    res("a")().isNaN shouldBe true
  }

  test("update after cyclic reference") {
    val a: Var[Expr] = Var { Plus(Ref("a"), Literal(1)) }
    val expressions = Map[String, Signal[Expr]](
      "a" -> a
    )
    val res = Calculator.computeValues(expressions)
    res("a")().isNaN shouldBe true

    a() = Literal(2)
    val res2 = Calculator.computeValues(expressions)
    res2("a")() shouldBe 2
  }
}
