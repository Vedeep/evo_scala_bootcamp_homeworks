package com.evobootcamp.homeworks.basics

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object ControlStructuresHomework {
  import Command._

  sealed trait Command {
    protected def result: Double
    protected def numbers: List[Double]
    protected def name: String = getClass.getSimpleName.toLowerCase
    def getResult: Result = CommandResult(name, result, numbers)
  }

  sealed trait Result {
    def command: String
    def value: Double
    def numbers: List[Double]
  }

  final case class ErrorMessage(value: String) {
    def getMessage: String = value
  }

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override protected def result: Double = dividend / divisor
      override protected val numbers: List[Double] = List(dividend, divisor)
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override protected def result: Double = numbers.sum
    }

    final case class Average(numbers: List[Double]) extends Command {
      override protected def result: Double = numbers.reduce(_ + _) / numbers.length
    }

    final case class Minimum(numbers: List[Double]) extends Command {
      override protected def result: Double = numbers.min
    }

    final case class Maximum(numbers: List[Double]) extends Command {
      override protected def result: Double = numbers.max
    }

    final case class CommandResult(command: String, value: Double, numbers: List[Double]) extends Result

    def createCommand(command: String): List[Double] => Either[ErrorMessage, Command] = { numbers: List[Double] =>
      command match {
        case "divide"  => numbers match {
          case  a :: b :: Nil => Right(Divide(a, b))
          case _              => Left(ErrorMessage("Invalid number of arguments, should be 2"))
        }
        case "sum"     => Right(Sum(numbers))
        case "average" => Right(Average(numbers))
        case "min"     => Right(Minimum(numbers))
        case "max"     => Right(Maximum(numbers))
        case _         => Left(ErrorMessage(s"Unsupported command - $command"))
      }
    }
  }

  object CommandParser {
    import Command.createCommand

    // just wanted to try regexp in scala
    // 2 regulars because i didn't find a method how we can get all values of duplicate groups
    private val commandPattern: Regex = "^([a-z]+)((?:\\s+-?[0-9]+\\.?(?:[0-9]+)?){2,})$".r
    private val argsPattern: Regex = "(-?[0-9]+\\.?(?:[0-9]+)?)".r

    def parse(x: String): Either[ErrorMessage, Command] = {
      x.trim match {
        case commandPattern(command, args)  => {
          val numbers = (for (m <- argsPattern.findAllMatchIn(args)) yield m.group(1).toDouble).toList
          createCommand(command)(numbers)
        }
        case _  => Left(ErrorMessage("Invalid input parameters"))
      }
    }
  }

  object Formatter {
    private def formatDouble(n: Double): String = if (n.toInt == n) n.toInt.toString else n.toString

    def formatResult(result: Result): String = result match {
      case CommandResult("divide", value, dividend :: divisor :: Nil)  => s"${formatDouble(dividend)} divided by ${formatDouble(divisor)} is ${formatDouble(value)}"
      case CommandResult(command, value, numbers) => s"the $command of ${numbers map formatDouble mkString " "} is ${formatDouble(value)}"
    }

    def formatError(error: ErrorMessage): String = s"Error: ${error.getMessage}"
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = CommandParser.parse(x)

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    Try(x.getResult) match {
      case Success(result)    => Right(result)
      case Failure(exception) => Left(ErrorMessage(exception.getMessage))
    }
  }

  def renderResult(x: Result): String = Formatter.formatResult(x)

  def renderError(e: ErrorMessage): String = s"Error: ${e.value}"

  def process(x: String): String = {
    (for {
      command   <- parseCommand(x)
      result    <- calculate(command)
    } yield result) match {
      case Left(error)    => renderError(error)
      case Right(result)  => renderResult(result)
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
