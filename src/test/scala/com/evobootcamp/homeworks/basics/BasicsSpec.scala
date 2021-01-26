package com.evobootcamp.homeworks.basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BasicsSpec extends AnyFlatSpec {
  "gcd" should "find greatest common divisor" in {
    gcd(8, 12) shouldEqual 4
    gcd(54, 24) shouldEqual 6
    gcd(42, 56) shouldEqual 14
    gcd(18, 84) shouldEqual 6
    gcd(48, 18) shouldEqual 6
    gcd(15, 0) shouldEqual 15
    gcd(0, 11) shouldEqual 11
    gcd(0, 0) shouldEqual 0
  }

  "lcd" should "find Lowest common denominator" in {
    lcd(6, 12) shouldEqual 12
    lcd(54, 24) shouldEqual 216
    lcd(42, 56) shouldEqual 168
    lcd(18, 84) shouldEqual 252
    lcd(48, 18) shouldEqual 144
    lcd(15, 0) shouldEqual 15
    lcd(0, 11) shouldEqual 11
    lcd(0, 0) shouldEqual 0
  }
}

