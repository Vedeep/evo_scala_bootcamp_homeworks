package com.evobootcamp.homeworks.errors

import com.evobootcamp.homeworks.errors.ErrorHandling.Homework.{PaymentCard}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import cats.syntax.all._
import com.evobootcamp.homeworks.errors.ErrorHandling.Homework.ValidationError._

class ErrorHandlingSpec extends AnyFlatSpec {
  "PaymentCardValidator" should "validate payment cards" in {
    import ErrorHandling.Homework.PaymentCardValidator._

    validate(
      "Vladislav Lipunov",
      "4276553377661212",
      "12/35",
      "123"
    ) shouldEqual PaymentCard(
      "Vladislav Lipunov",
      "4276553377661212",
      "12/35",
      "123"
    ).validNec

    validate(
      "",
      "427655337766",
      "01/10",
      ""
    ) shouldEqual
      NameIsEmpty.invalidNec
        .product(NameFormatInvalid.invalidNec)
        .product(NumberInvalid.invalidNec)
        .product(ExpirationDateInvalid.invalidNec)
        .product(SecurityCodeInvalid.invalidNec)

    validate(
      "Vladislav Lipunov",
      "4276553377661212",
      "fo/rmat",
      "123"
    ) shouldEqual ExpirationFormatInvalid.invalidNec
  }
}
