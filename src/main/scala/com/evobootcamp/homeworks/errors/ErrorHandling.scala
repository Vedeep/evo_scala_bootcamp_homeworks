package com.evobootcamp.homeworks.errors

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.util.{Failure, Success, Try}

object ErrorHandling extends App {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  object Homework {
    import cats.data.ValidatedNec
    import cats.syntax.all._

    case class PaymentCard(name: String, number: String, expirationDate: String, securityCode: String)

    sealed trait ValidationError
    object ValidationError {
      final case object NumberInvalid extends ValidationError {
        override def toString: String = "The card number must consist of 16 digits"
      }

      final case object ExpirationFormatInvalid extends ValidationError {
        override def toString: String = "The expiration date format should be xx/xx"
      }

      final case object ExpirationDateInvalid extends ValidationError {
        override def toString: String = "The expiration date can not be less than now"
      }

      final case object NameIsEmpty extends ValidationError {
        override def toString: String = "The name cannot be empty"
      }

      final case object NameFormatInvalid extends ValidationError {
        override def toString: String = "The name should have characters A-z, 0-9 and a space"
      }

      final case object SecurityCodeInvalid extends ValidationError {
        override def toString: String = "The security code must consist of 3 digits"
      }
    }

    object PaymentCardValidator {

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      val dateFormat = new SimpleDateFormat("MM/yy")

      def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String,
      ): AllErrorsOr[PaymentCard] = {
        val card = PaymentCard(name, number, expirationDate, securityCode)

        def validateName: AllErrorsOr[PaymentCard] =
          if (name.length == 0) ValidationError.NameIsEmpty.invalidNec
          else card.validNec

        def validateNameFormat: AllErrorsOr[PaymentCard] =
          if (name.matches("^[A-z0-9\\s]+$")) card.validNec
          else ValidationError.NameFormatInvalid.invalidNec

        def validateNumber: AllErrorsOr[PaymentCard] =
          if (number.matches("^[0-9]{16}$")) card.validNec
          else ValidationError.NumberInvalid.invalidNec

        def validateExpirationDate: AllErrorsOr[PaymentCard] = {
          Try(dateFormat.parse(expirationDate)) match {
            case Success(v) =>
              if (v.after(Calendar.getInstance().getTime()))
                card.validNec
              else
                ValidationError.ExpirationDateInvalid.invalidNec

            case Failure(_) => ValidationError.ExpirationFormatInvalid.invalidNec
          }
        }

        def validateSecurityCode: AllErrorsOr[PaymentCard] =
          if (securityCode.matches("^[0-9]{3}$")) card.validNec
          else ValidationError.SecurityCodeInvalid.invalidNec

        validateName *>
          validateNameFormat *>
          validateNumber *>
          validateExpirationDate *>
          validateSecurityCode
      }
    }
  }

  // Attributions and useful links:
  // https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html#error-handling
  // https://www.geeksforgeeks.org/scala-exception-handling/
  // https://typelevel.org/cats/datatypes/validated.html
  // https://blog.ssanj.net/posts/2019-08-18-using-validated-for-error-accumulation-in-scala-with-cats.html
}
