package com.evobootcamp.homeworks.testing

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Test.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  def genJBoolean: Gen[JBoolean] = for {
    v <- Gen.oneOf(true, false)
  } yield JBoolean(v)

  def genJNumber: Gen[JNumber] = for {
    v <- Arbitrary.arbitrary[Int]
  } yield JNumber(v)

  def genJString: Gen[JString] = for {
    v <- Gen.alphaStr
  } yield JString(v)

  def genJObjectItem: Arbitrary[(String, Json)] = Arbitrary {
    for {
      key <- Gen.identifier
      value <- Gen.oneOf[Json](genJBoolean, genJNumber, genJString)
    } yield (key, value)
  }

  def genJObject: Gen[JObject] = for {
    items <- Gen.mapOf(genJObjectItem.arbitrary)
  } yield JObject(items)

  def genJArray: Gen[JArray] = for {
    items <- Gen.someOf(genJBoolean, genJNumber, genJString, genJObject)
  } yield JArray(items.toVector)

  def jsonGen: Gen[Json] = Gen.oneOf(genJArray, genJObject)

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      println("is print", print(json))
      parse(print(json)) shouldEqual Some(json)
    }
  }
}
