package com.knoldus

import com.typesafe.scalalogging.Logger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Driver extends App {
  private val loggers = Logger(getClass)
  private val calculator = Calculator
  private val extraMethods = new ExtraMethods

  // Calling the calculate method to perform Addition
  private val sum = calculator.calculate("+", Seq(10.0, 20.0))
  sum.onComplete {
    case Success(value) => loggers.info("Sum :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Subtraction
  private val sub = calculator.calculate("-", Seq(30.0, 90.0))
  sub.onComplete {
    case Success(value) => loggers.info("Subtract :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Multiplication
  private val multiply = calculator.calculate("*", Seq(2.0, 5.0))
  multiply.onComplete {
    case Success(value) => loggers.info("Multiplication :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Divide
  private val divide = calculator.calculate("/", Seq(2.0, 5.0))
  divide.onComplete {
    case Success(value) => loggers.info("Division" + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Power
  private val pow = calculator.calculate("^", Seq(2.0, 5.0))
  pow.onComplete {
    case Success(value) => loggers.info("Power :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform SquareRoot
  private val sqrt = calculator.calculate("sqrt", Seq(5.0))
  sqrt.onComplete {
    case Success(value) => loggers.info("Square Root :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Factorial
  private val factorial = calculator.calculate("!", Seq(5.0))
  factorial.onComplete {
    case Success(value) => loggers.info("Factorial :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform ListSum
  private val listSum = calculator.calculate("sum", Seq(5.0, 6.0, 5.0))
  listSum.onComplete {
    case Success(value) => loggers.info("List Sum :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to perform Greatest Common Divisor
  private val gcd = calculator.calculate("gcd", Seq(5.0, 40.0))
  gcd.onComplete {
    case Success(value) => loggers.info("GCD :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to find the odd number in the Sequence
  private val oddInSequence = calculator.calculate("odd", Seq(5.0, 10.0, 12.0, 19.0))
  oddInSequence.onComplete {
    case Success(value) => loggers.info("Odd Numbers :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to find the odd number in the Sequence
  private val evenInSequence = calculator.calculate("even", Seq(5.0))
  evenInSequence.onComplete {
    case Success(value) => loggers.info("Even Numbers :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //Calling the calculate method to find the Fibonacci Sequence
  private val fibonacciSequence = calculator.calculate("fibonacci", Seq(5.0))
  fibonacciSequence.onComplete {
    case Success(value) => loggers.info("Fibonacci Series :- " + value)
    case Failure(exception) => loggers.warn("Error ", exception)
  }

  //squareOfExpression -
  // This method will check if (firstOperand + secondOperand) ^ 2 == (firstOperand ^ 2) + (secondOperand ^ 2) + (2 * firstOperand * secondOperand).
  // If true, return “Equal” else “Not Equal”
  private val squareExpression = extraMethods.squareOfExpression(0.0, 1.0)
  println(squareExpression)

  //find - Find the numbers from the provided sequence whose factorial is greater than 6 ^ number
  private val find = extraMethods.find(Seq(5.0, 10.0, 14.0, 23.0))
  find.onComplete {
    case Success(value) => loggers.info("Find:- " + value)
    case Failure(exception) => loggers.warn("Error " + exception)
  }

  //findAverageAfterChainingOperations -
  //For each number in the numbers sequence first find the fibonacci then find the odd numbers from the resulting sequence and then return their sum.
  //After you have performed the above operation on each number, find the average.
  private val findAverageAfterChaining = extraMethods.findAverageAfterChainingOperations(Seq(13.0, 15.0))
  findAverageAfterChaining.onComplete {
    case Success(value) => loggers.info("After Chaining :- " + value)
    case Failure(exception) => loggers.warn("Error " + exception)
  }

}