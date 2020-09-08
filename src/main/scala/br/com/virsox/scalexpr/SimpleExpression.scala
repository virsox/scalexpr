package br.com.virsox.scalexpr

import java.time.Instant

import br.com.virsox.scalexpr.Context.{ContextLike, EmptyContext}

/** Simple / unary expressions. */
trait SimpleExpression[T] extends Expression[T]

/** Constant expression */
trait Constant[T] extends SimpleExpression[T] {
  def value: T

  /** *
    * Simply returns the constant value.
    * @param context Not used.
    * @return Value of the constant.
    */
  override def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): T = value
}

case class BooleanConstant(value: Boolean)  extends BooleanExpr with Constant[Boolean]
case class IntConstant(value: Int)          extends NumericExpr[Int] with Constant[Int]
case class LongConstant(value: Long)        extends NumericExpr[Long] with Constant[Long]
case class DoubleConstant(value: Double)    extends NumericExpr[Double] with Constant[Double]
case class StringConstant(value: String)    extends OrderingExpr[String] with Constant[String]
case class DateTimeConstant(value: Instant) extends OrderingExpr[Instant] with Constant[Instant]

/** Represents a variable. */
trait Variable[T] extends SimpleExpression[T] {

  def name: String

  /**
    * Resolves the value of the variable using the context map as source. The map should contain a key
    * with the variable name. The method also accepts nested maps. For instance, for a variable named
    * "test1.test2", the method searches for a "test1" key in the parameter map, and for a "test2" key
    * in the "test1" map.
    * @param context Context map containing the variable values.
    * @return Value of the variable.
    */
  override def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): T = {
    val tokens                  = name.split("\\.")
    val (eventNames, attribute) = tokens.splitAt(tokens.length - 1)

    val lastEventOption =
      eventNames.foldLeft(Some(context): Option[A])((ctx, field) => ctx.flatMap(c => ctxLike.nested(c, field)))

    lastEventOption match {
      case Some(ctx) =>
        ctxLike.get[T](ctx, attribute(0)) match {
          case Some(value) => value
          case None        => throw new IllegalArgumentException("variable not found in context")
        }
      case None => throw new IllegalArgumentException("variable not found in context")
    }
  }
}

case class BooleanVar(name: String)  extends BooleanExpr with Variable[Boolean]
case class IntVar(name: String)      extends NumericExpr[Int] with Variable[Int]
case class LongVar(name: String)     extends NumericExpr[Long] with Variable[Long]
case class DoubleVar(name: String)   extends NumericExpr[Double] with Variable[Double]
case class StringVar(name: String)   extends OrderingExpr[String] with Variable[String]
case class DateTimeVar(name: String) extends OrderingExpr[Instant] with Variable[Instant]
