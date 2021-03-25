package com.evobootcamp.homeworks.testing

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import java.lang.reflect.Method

class CalculatorSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks  {
  import Calculator._
  import MathOperation._

  def getChainMethod(): Method = {
    val chainClass = classOf[MathChain]
    val chainAddMehod = chainClass.getDeclaredMethod("chainAdd", classOf[Traits.MathOperation])
    chainAddMehod.setAccessible(true)

    chainAddMehod
  }

  "Math.chain" should "return a MathChain object" in {
    Math.chain(1) shouldEqual MathChain(1)
  }

  "MathChain" should "return init variable when the list is empty" in {
    Math.chain(1).done shouldEqual 1
  }

  "MathChain.chainAdd" should "returns a copy of MathChain" in {
    val chain = Math.chain(0)
    val chainAddMehod = getChainMethod()
    val newChain = chainAddMehod.invoke(chain, Sum(0))

    (newChain eq chain) shouldBe false
    newChain shouldEqual Math.chain(0).add(0)
  }

  "MathChain operations" should "add MathOperation item to a chain" in {
    val chain = Math.chain(9).add(10).subtract(11).multiply(12).divide(13)
    val items = List(
      MathChainItem(Divide(13)),
      MathChainItem(Multiply(12)),
      MathChainItem(Subtract(11)),
      MathChainItem(Sum(10)),
    )
    chain shouldEqual MathChain(9, items)
  }

  "Sum" should "calculate add numbers" in {
    forAll { (a: Double, b: Double) =>
      Sum(a).calculate(b) shouldBe a + b
    }
  }

  "Subtract" should "subtract numbers" in {
    forAll { (a: Double, b: Double) =>
      Subtract(a).calculate(b) shouldBe b - a
    }
  }

  "Multiply" should "multiply numbers" in {
    forAll { (a: Double, b: Double) =>
      Multiply(a).calculate(b) shouldBe b * a
    }
  }

  "Divide" should "divide numbers" in {
    forAll { (a: Double, b: Double) =>
      Divide(a).calculate(b) shouldBe b / a
    }
  }

  "Math" should "return correct result" in {
    forAll { (a: Double, b: Double, c: Double, d: Double, e: Double) =>
      Math.chain(a).multiply(b).add(c).divide(d).subtract(e).done shouldBe (a * b + c) / d - e
    }
  }
}
