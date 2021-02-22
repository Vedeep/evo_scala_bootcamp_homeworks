package com.evobootcamp.homeworks.typeclass

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HomeWorkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Money" should "have default ordering" in {
    import Task1._

    implicitly[Ordering[Money]] shouldBe moneyOrdering
  }

  "User" should "can show" in {
    import Task2._
    import Task2.Showable._

    forAll { (s: String, s2: String) =>
      User(s, s2).show shouldEqual (s, s2).toString
    }
  }

  "User" should "parse from string" in {
    import Task3._
    import Task3.Parsable._

    "123,User Name".parse[User] shouldEqual Right(User("123", "User Name"))
    "123,User Name, User Lastname".parse[User] shouldEqual Left("Not parsable")
  }

  "Entity" should "has '===' method" in {
    import Task4._

    sealed case class Test(a: String, b: Int)

    EqualsSyntax("123") === "123" shouldBe true
    EqualsSyntax("123") === "321" shouldBe false
    EqualsSyntax(123) === 123 shouldBe true
    EqualsSyntax(567) === 123 shouldBe false

    EqualsSyntax(Test("123", 456)) === Test("123", 456) shouldBe true
    EqualsSyntax(Test("123", 456)) === Test("456", 123) shouldBe false
  }

  "Iterable" should "has myFlatMap method" in {
    import AdvancedHomework._

    List(1, 2, 3).myFlatMap(x => List(x + 2)) shouldEqual List(1, 2, 3).flatMap(x => List(x + 2))
    List(1, 2, 3).myFlatMap(List(_)) shouldEqual List(1, 2, 3).flatMap(List(_))
  }
}
