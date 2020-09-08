package br.com.virsox.scalexpr

import br.com.virsox.scalexpr.Context.{ContextLike, EmptyContext}

import scala.math.Ordering
import scala.reflect.runtime.universe._

/** *
  * Trait for binary expressions.
  * @tparam A Type of the left operand.
  * @tparam B Type of the right operand.
  * @tparam T Type of result (and consequently, of the expression).
  */
trait BinaryExpression[A, B, T] extends Expression[T] {
  val left: Expression[A]
  val right: Expression[B]
}

// ---------------------------------------------------------------------------------------------------------

/** Relational operators objects. */
class RelationalOperator
object Equals            extends RelationalOperator
object NotEquals         extends RelationalOperator
object LessThan          extends RelationalOperator
object LessEqualsThan    extends RelationalOperator
object GreaterThan       extends RelationalOperator
object GreaterEqualsThan extends RelationalOperator

/**
  * Relational expression. Both operands must be of the same type. This expression always return a boolean.
  * @param left Left operand.
  * @param op Comparison to be made.
  * @param right Right operand.
  * @tparam T Type of the operands.
  */
case class RelationalExpression[T: Ordering](left: Expression[T], op: RelationalOperator, right: Expression[T])
    extends BooleanExpr
    with BinaryExpression[T, T, Boolean] {

  /** Ordering object. */
  val ordering: Ordering[T] = implicitly[Ordering[T]]

  /**
    * Resolves the relational expression.
    * @param context Not used directly by the expression, but it is passed recursively to resolve the operands.
    * @return Value of the expression (always a boolean).
    */
  override def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): Boolean =
    op match {
      case Equals            => left.resolve(context) == right.resolve(context)
      case NotEquals         => left.resolve(context) != right.resolve(context)
      case LessThan          => ordering.lt(left.resolve(context), right.resolve(context))
      case LessEqualsThan    => ordering.lteq(left.resolve(context), right.resolve(context))
      case GreaterThan       => ordering.gt(left.resolve(context), right.resolve(context))
      case GreaterEqualsThan => ordering.gteq(left.resolve(context), right.resolve(context))
    }
}

// ---------------------------------------------------------------------------------------------------------

/** Logical operators. */
class LogicalOperator
object And extends LogicalOperator
object Or  extends LogicalOperator

/**
  * Logical expression. Both operands must be booleans expressions, and the result is also a boolean.
  * @param left Left operand.
  * @param op Logical operation.
  * @param right Right operand.
  */
case class LogicalExpression(left: Expression[Boolean], op: LogicalOperator, right: Expression[Boolean])
    extends BooleanExpr
    with BinaryExpression[Boolean, Boolean, Boolean] {

  /**
    * Resolves the logical operation.
    * @param context Not used directly by the expression, but it is passed recursively to resolve the operands.
    * @return Value of the expression.
    */
  override def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): Boolean =
    op match {
      case And => left.resolve(context) && right.resolve(context)
      case Or  => left.resolve(context) || right.resolve(context)
    }
}

// ---------------------------------------------------------------------------------------------------------

/** Arithmetic operators. */
class ArithmeticOperator
object Plus  extends ArithmeticOperator
object Minus extends ArithmeticOperator
object Times extends ArithmeticOperator
object Div   extends ArithmeticOperator

/** *
  * Arithmetic operation. Both operands must be of the same type. Expression results in the same type.
  * @param left Left operand.
  * @param op Arithmetic operation.
  * @param right Right operand.
  * @tparam T Type of the operands and return type.
  */
case class ArithmeticExpression[T: Numeric: TypeTag](left: Expression[T], op: ArithmeticOperator, right: Expression[T])
    extends NumericExpr[T]
    with BinaryExpression[T, T, T] {

  /**
    * Resolves the arithmetic operation.
    * @param context Not used directly by the expression, but it is passed recursively to resolve the operands.
    * @return Value of the expression.
    */
  override def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): T =
    op match {
      case Plus  => numericEvidence.plus(left.resolve(context), right.resolve(context))
      case Minus => numericEvidence.minus(left.resolve(context), right.resolve(context))
      case Times => numericEvidence.times(left.resolve(context), right.resolve(context))
      case Div   =>
        // division is not defined in the Numeric type
        if (typeOf[T] =:= typeOf[Double]) {
          (numericEvidence.toDouble(left.resolve(context)) / numericEvidence.toDouble(right.resolve(context))).asInstanceOf[T]
        } else if (typeOf[T] =:= typeOf[Float]) {
          (numericEvidence.toFloat(left.resolve(context)) / numericEvidence.toFloat(right.resolve(context))).asInstanceOf[T]
        } else if (typeOf[T] =:= typeOf[Long]) {
          (numericEvidence.toLong(left.resolve(context)) / numericEvidence.toLong(right.resolve(context))).asInstanceOf[T]
        } else {
          (numericEvidence.toInt(left.resolve(context)) / numericEvidence.toInt(right.resolve(context))).asInstanceOf[T]
        }
    }

}
