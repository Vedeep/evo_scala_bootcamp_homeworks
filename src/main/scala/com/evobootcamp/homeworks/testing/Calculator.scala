package com.evobootcamp.homeworks.testing

object Calculator {

  object Traits {
    sealed trait MathOperation {
      def rightOperand: Double
      def calculate(leftOperand: Double): Double
    }

    sealed trait MathChainItem {
      def operation: MathOperation
    }

    sealed trait MathChain {
      def add(v: Double): MathChain
      def subtract(v: Double): MathChain
      def divide(v: Double): MathChain
      def multiply(v: Double): MathChain
      def done: Double
    }

    sealed trait Math {
      def chain(variable: Double): MathChain
    }
  }

  object MathOperation {
    final case class Sum(rightOperand: Double) extends Traits.MathOperation {
      override def calculate(leftOperand: Double): Double = leftOperand + rightOperand
    }
    final case class Subtract(rightOperand: Double) extends Traits.MathOperation {
      override def calculate(leftOperand: Double): Double = leftOperand - rightOperand;
    }
    final case class Divide(rightOperand: Double) extends Traits.MathOperation {
      override def calculate(leftOperand: Double): Double = leftOperand / rightOperand
    }
    final case class Multiply(rightOperand: Double) extends Traits.MathOperation {
      override def calculate(leftOperand: Double): Double = leftOperand * rightOperand
    }
  }

  import MathOperation._

  final case class MathChainItem(operation: Traits.MathOperation) extends Traits.MathChainItem

  final case class MathChain(
    private val startVar: Double,
    private val chain: List[MathChainItem] = List()
  ) extends Traits.MathChain {

    private def chainAdd(operation: Traits.MathOperation): MathChain = {
      this.copy(chain = MathChainItem(operation) :: chain)
    }

    override def add(v: Double): MathChain = chainAdd(Sum(v))
    override def subtract(v: Double): MathChain = chainAdd(Subtract(v))
    override def divide(v: Double): MathChain = chainAdd(Divide(v))
    override def multiply(v: Double): MathChain = chainAdd(Multiply(v))

    override def done: Double = chain.foldRight(startVar)((item, acc) => item.operation.calculate(acc))

  }

  final object Math extends Traits.Math {
    override def chain(variable: Double): MathChain = MathChain(variable)
  }
}
