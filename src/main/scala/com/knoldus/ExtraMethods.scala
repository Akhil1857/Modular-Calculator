package com.knoldus

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.math.pow

class ExtraMethods {

  //  squareOfExpression -
  //  This method will check if (firstOperand + secondOperand) ^ 2 == (firstOperand ^ 2) + (secondOperand ^ 2) + (2 * firstOperand * secondOperand).
  //  If true, return “Equal” else “Not Equal”
  def squareOfExpression(firstOperand: Double, secondOperand: Double): String = {
    Try {
      val add = new Add
      val power = new Power
      val mul = new Multiplication
      val list = Seq(firstOperand, secondOperand)
      val lhsSide = power.validateAndExecute(add.validateAndExecute(list) ++ Seq(2.0))
      val RhsHalfSide = add.validateAndExecute(power.validateAndExecute(Seq(firstOperand, 2)) ++ power.validateAndExecute(Seq(secondOperand, 2)))
      val RhsTotal = add.validateAndExecute(RhsHalfSide ++ mul.validateAndExecute(mul.validateAndExecute(Seq(2, firstOperand)) ++ Seq(secondOperand)))
      val result = lhsSide == RhsTotal
      result
    }
    match {
      case Success(value) => if (value) "Equal" else "Not Equal"
      case Failure(exception) => s"Error $exception"
    }
  }

  //find - Find the numbers from the provided sequence whose factorial is greater than 6 ^ number
  def find(numbers: Seq[Double]): Future[Seq[Double]] = {

    Future {
      @tailrec
      def findFactorial(number: Double, result: Double): Double = {
        if (number <= 1) result
        else findFactorial(number - 1, result * number)
      }

      val finalResult = numbers.filter { num =>
        val result = findFactorial(num, 1)
        result > pow(6, num)
      }
      finalResult
    }
  }

  //findAverageAfterChainingOperations -
  //For each number in the numbers sequence first find the fibonacci then find the odd numbers from the resulting sequence and then return their sum.
  //After you have performed the above operation on each number, find the average.
  def findAverageAfterChainingOperations(numbers: Seq[Double]): Future[Double] = {
    Future {
      @tailrec
      def fibonacci(times: Double, firstNumber: Double, SecondNumber: Double): Double = {
        if (times <= 1) SecondNumber
        else fibonacci(times - 1, SecondNumber, firstNumber + SecondNumber)
      }

      val filteredNumbers = numbers.filter { num =>
        val res = fibonacci(num.toInt, 0, 1)
        res % 2 != 0
      }
      filteredNumbers.foldLeft(0.0)((numOne: Double, numTwo: Double) => numOne + numTwo) / filteredNumbers.size
    }
  }
}