package com.evobootcamp.homeworks.typeclass

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OopComparisonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "hash" should "be equal to hashCode" in {
    import TypeclassTask.Hashable._

    forAll { s: String =>
      s.hash shouldEqual s.hashCode
    }
  }
}

