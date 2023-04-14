package com.knoldus

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.math.{pow, sqrt}


class CalculatorException extends Exception("Failed")

class Add extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    operands.size == 2
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head + operands(1))

}

class Subtraction extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 2

  override def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head - operands(1))
}

class Multiplication extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 2

  override def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head * operands(1))

}

class Division extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 2

  override def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head / operands(1))

}

class Power extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 2

  override def execute(operands: Seq[Double]): Seq[Double] = Seq(pow(operands.head, operands(1)))

}


class SquareRoot extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 1 && operands.head >= 0

  override def execute(operands: Seq[Double]): Seq[Double] = Seq(sqrt(operands.head))

}

class Factorial extends Operator {

  override def validate(operands: Seq[Double]): Boolean = operands.size == 1 && operands.head >= 0

  override def execute(operands: Seq[Double]): Seq[Double] = {
    def factorial(number: Double): Double = {
      @tailrec
      def helper(digit: Int, accumulator: Int): Double = {
        if (digit > 0) helper(digit - 1, accumulator * digit)
        else accumulator
      }

      helper(number.toInt, 1)
    }

    Seq(factorial(operands.head))
  }
}

class Sum extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.nonEmpty

  override def execute(operands: Seq[Double]): Seq[Double] = {
    val sum = operands.fold(0.0)(_ + _)
    Seq(sum)
  }
}

class Gcd extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.size == 2

  override def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def gcd(first: Double, second: Double): Double = {
      if (second == 0) first
      else gcd(second, first % second)
    }

    Seq(gcd(operands.head, operands(1)))
  }
}

class Even extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.nonEmpty

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filter(_ % 2 == 0)
  }
}

class Odd extends Operator {
  override def validate(operands: Seq[Double]): Boolean = operands.nonEmpty

  override def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filter(_ % 2 != 0)
  }

}

class Fibonacci extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1) true
    else false
  }

  override def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def fibonacci(number: Double, first: Double, second: Double, list: Seq[Double]): Seq[Double] = {
      if (number == 0) list
      else fibonacci(number - 1, second, second + first, list :+ first)
    }

    fibonacci(operands.head, 0, 1, Seq())
  }
}


object Calculator {
  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    operator match {
      case "+" => execute(new Add, operands)
      case "-" => execute(new Subtraction, operands)
      case "*" => execute(new Multiplication, operands)
      case "/" => execute(new Division, operands)
      case "^" => execute(new Power, operands)
      case "sqrt" => execute(new SquareRoot, operands)
      case "!" => execute(new Factorial, operands)
      case "sum" => execute(new Sum, operands)
      case "gcd" => execute(new Gcd, operands)
      case "odd" => execute(new Odd, operands)
      case "even" => execute(new Even, operands)
      case "fibonacci" => execute(new Fibonacci, operands)

      case _ => Future.failed(new IllegalArgumentException(s"Unknown operator: $operator"))
    }
  }

  def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = {
    if (operator.validate(operands)) {
      Future.successful(operator.validateAndExecute(operands))
    }
    else {
      Future.failed(new Exception("Invalid"))
    }
  }
}


