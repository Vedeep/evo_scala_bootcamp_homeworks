package com.evobootcamp.homeworks.effects

import com.evobootcamp.homeworks.effects.EffectsHomework1.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.{Failure, Success}
import scala.util.control.NoStackTrace

class EffectsHomework1Spec extends AnyFlatSpec {
  private object TestException extends Exception with NoStackTrace

  "IO.map" should "be lazy" in {
    var called = false

    val b = IO(1).map(_a => {
      called = true
      _a + 1
    })

    called shouldBe false

    b.unsafeRunSync() shouldBe 2
    called shouldBe true
  }

  "IO.flatMap" should "be lazy" in {
    var called = false

    val b = IO(1).flatMap { a =>
      called = true
      IO(a + 3)
    }

    called shouldBe false

    b.unsafeRunSync() shouldBe 4
    called shouldBe true
  }

  "IO.*>" should "be called after main body" in {
    var called = false
    lazy val body = {
      called = true
      2
    }

    val b = IO(1).flatMap(_ => IO.raiseError[Int](TestException)).*>(IO(body))

    called shouldBe false

    intercept[TestException.type] {
      b.unsafeRunSync()
    }

    called shouldBe false

    (IO(1) *> IO(body)).unsafeRunSync shouldBe 2
    called shouldBe true
  }

  "IO.void" should "change value to Unit" in {
    IO(1).map(_ + 1).void.unsafeRunSync shouldBe ()
  }

  "IO.attempt" should "casts exception to either" in {
    IO.raiseError(TestException).attempt.unsafeRunSync() shouldEqual Left(TestException)
    IO.pure(123).attempt.unsafeRunSync() shouldEqual Right(123)
  }

  "IO.option" should "result in Option" in {
    IO.raiseError(TestException).option.unsafeRunSync() shouldEqual None
    IO.pure(123).option.unsafeRunSync() shouldBe Some(123)
  }

  "IO.handleErrorWith" should "catch exceptions" in {
    IO.raiseError(TestException).handleErrorWith({
      case TestException => IO.pure(123)
      case _ => IO.pure(456)
    }).unsafeRunSync() shouldBe 123

    IO.raiseError(new Exception("Test")).handleErrorWith({
      case TestException => IO.pure(123)
      case _ => IO.pure(456)
    }).unsafeRunSync() shouldBe 456

    IO.pure(789).handleErrorWith({
      case TestException => IO.pure(123)
      case _ => IO.pure(456)
    }).unsafeRunSync() shouldBe 789
  }

  "IO.redeem" should "catch exceptions or map result" in {
    IO.raiseError[Int](TestException).redeem({
      case TestException => 123
    }, _ => 456).unsafeRunSync() shouldBe 123

    IO.pure(789).redeem({
      case TestException => IO.pure(123)
    }, _ => 456).unsafeRunSync() shouldBe 456
  }

  "IO.redeemWith" should "catch exceptions or flatMap result" in {
    IO.raiseError[Int](TestException).redeemWith({
      case TestException => IO.pure(123)
    }, _ => IO.pure(456)).unsafeRunSync() shouldBe 123

    IO.pure(789).redeemWith({
      case TestException => IO.pure(123)
    }, _ => IO.pure(456)).unsafeRunSync() shouldBe 456
  }

  "IO.unsafeRunSync" should "execute body and returns result" in {
    var called = false
    lazy val body = {
      called = true
      1 + 1 + 1
    }

    val a = IO(body)

    called shouldBe false
    a.unsafeRunSync() shouldBe 3
    called shouldBe true
  }

  "IO.unsafeToFuture" should "return future" in {
    IO.raiseError[Int](TestException).unsafeToFuture().value shouldEqual Some(Failure(TestException))
    IO.pure(123).unsafeToFuture().value shouldBe Some(Success(123))
  }

  "IO.apply" should "returns a new instance of IO with body argument as lazy" in {
    var called = false
    lazy val a = {
      called = true
      123
    }

    val io = IO.apply(a)

    called shouldBe false
    io.run() shouldBe 123
  }

  "IO.suspend" should "return a new instance of IO" in {
    IO.suspend(IO(123)).unsafeRunSync() shouldEqual 123
  }

  "IO.delay" should "be equal to apply" in {
    IO.delay("test").unsafeRunSync() shouldEqual "test"
  }

  "IO.pure" should "wrap value in IO" in {
    IO.pure(123).run() shouldBe 123
  }

  "IO.fromEither" should "raise an exception or return a pure value of IO" in {
    intercept[TestException.type] {
      IO.fromEither(Left(TestException)).unsafeRunSync()
    }

    IO.fromEither(Right("test")).unsafeRunSync() shouldBe "test"
  }

  "IO.fromOption" should "returns ф pure value or raise an error if None" in {
    intercept[TestException.type] {
      IO.fromOption(None)(TestException).unsafeRunSync()
    }

    IO.fromOption(Some("test"))(TestException).unsafeRunSync() shouldBe "test"
  }

  "IO.fromTry" should "return a pure IO or raise an error" in {
    intercept[TestException.type] {
      IO.fromTry(Failure(TestException)).unsafeRunSync()
    }

    IO.fromTry(Success("test")).unsafeRunSync() shouldBe "test"
  }

  "IO.none" should "returns a wrapped IO[None]" in {
    IO.none.unsafeRunSync() shouldEqual None
  }

  "IO.raiseError" should "throw an exception" in {
    intercept[TestException.type] {
      IO.raiseError(TestException).unsafeRunSync()
    }
  }

  "IO.raiseUnless" should "should throw an exception if a condition is false" in {
    intercept[TestException.type] {
      IO.raiseUnless(false)(TestException).unsafeRunSync()
    }

    IO.raiseUnless(true)(TestException).unsafeRunSync() shouldBe ()
  }

  "IO.raiseWhen" should "should throw an exception if a condition is true" in {
    intercept[TestException.type] {
      IO.raiseWhen(true)(TestException).unsafeRunSync()
    }

    IO.raiseWhen(false)(TestException).unsafeRunSync() shouldBe ()
  }

  "IO.unlessA" should "execute an action if a condition is false" in {
    intercept[TestException.type] {
      IO.unlessA(false)(IO.raiseError(TestException)).unsafeRunSync()
    }

    IO.unlessA(true)(IO.raiseError(TestException)).unsafeRunSync() shouldBe ()
  }

  "IO.whenA" should "execute an action if a condition is false" in {
    intercept[TestException.type] {
      IO.whenA(true)(IO.raiseError(TestException)).unsafeRunSync()
    }

    IO.whenA(false)(IO.raiseError(TestException)).unsafeRunSync() shouldBe ()
  }

  "IO.unit" should "returns IO with the unit body" in {
    IO.unit.unsafeRunSync() shouldEqual ()
  }
}
