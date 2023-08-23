package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructures.d
import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework2.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source
import scala.tools.nsc.profile.EventType.value
import scala.util.Left

object ControlStructuresHomework2 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result
  case class DivideResult(dividend: Double, divisor: Double, result: Double) extends Result {
    override def toString(): String = {
      s"$dividend divided by $divisor is $result"
    }
  }
  case class SumResult(initValues: List[Double], result: Double) extends Result {
    override def toString(): String = {
      s"the sum of ${initValues.mkString(" ")} is $result"
    }
  }
  case class AverageResult(initValues: List[Double], result: Double) extends Result {
    override def toString(): String = {
      s"the average of ${initValues.mkString(" ")} is $result"
    }
  }
  case class MinResult(initValues: List[Double], result: Double) extends Result {
    override def toString(): String = {
      s"the minimum of ${initValues.mkString(" ")} is $result"
    }
  }
  case class MaxResult(initValues: List[Double], result: Double) extends Result {
    override def toString(): String = {
      s"the maximum of ${initValues.mkString(" ")} is $result"
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val splitted: List[String] = x.trim.replaceAll(" +", " ").split(" ").toList
    splitted match {
      case "divide" :: dividend :: divisor :: Nil => Right(Divide(dividend.toDouble, divisor.toDouble))
      case "sum" :: numbers => Right(Sum(numbers.map(_.toDouble)))
      case "average" :: numbers => Right(Average(numbers.map(_.toDouble)))
      case "min" :: numbers => Right(Min(numbers.map(_.toDouble)))
      case "max" :: numbers => Right(Max(numbers.map(_.toDouble)))
      case _ :: Nil => Left(ErrorMessage("Error: Arguments cannot be empty"))
      case _ => Left(ErrorMessage("Error: Incorrect parameters"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) => if (divisor == 0) Left(ErrorMessage("Error: Cannot divide by zero")) else Right(DivideResult(dividend, divisor, dividend / divisor))
      case Sum(numbers) => Right(SumResult(numbers, numbers.sum))
      case Average(numbers) => Right(AverageResult(numbers, numbers.sum / numbers.length))
      case Min(numbers) => Right(MinResult(numbers, numbers.min))
      case Max(numbers) => Right(MaxResult(numbers, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x.toString
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
//    for {
//      command <- parseCommand(x)
//      result <- calculate(command)
////      processed <- renderResult(result)
//
//    } yield result.ri
    // implement using a for-comprehension
    ???
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
