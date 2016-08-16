package br.com.virsox.scalexpr

import java.time.Instant

import br.com.virsox.scalexpr.Context.{ContextLike, EmptyContext}

import scala.math.Ordering
import scala.reflect.runtime.universe._


/** Encapsulate implicits used by the scalexpr package. */
object Implicits {

  /** Establishes an ordering between two instants. */
  trait InstantOrdering extends Ordering[Instant] {
    def compare(x: Instant, y: Instant) = x.compareTo(y)
  }

  /** Implicit object that implements the ordering. */
  implicit object InstantOrderingObj extends InstantOrdering
}


/**
  * Expression of a certain type.
  * @tparam T Type of the expression.
  */
trait Expression[T] {

  /** TypeTag representing the type of the expression. */
  implicit val typeTagEvidence: TypeTag[T]

  /**
    * Resolves the expression value.
    * @param context Contains information that can be used for resolving the expression.
    * @return Value of the expression.
    */
  def resolve[A](context: A = EmptyContext)(implicit ctxLike: ContextLike[A]): T
}


/** Trait containing operations for boolean expressions. */
trait BooleanMethods {this: Expression[Boolean] =>
  /**
    * Logical AND.
    * @param rhs Right-hand side of the AND operation.
    * @return Expression representing the AND between the two operands.
    */
  def &&(rhs: Expression[Boolean]) = new LogicalExpression(this, And, rhs)

  /**
    * Logical OR.
    * @param rhs Right-hand side of the OR operation.
    * @return Expression representing the OR between the two operands.
    */
  def ||(rhs: Expression[Boolean]) = new LogicalExpression(this, Or, rhs)
}


/**
  * Trait containing methods for expressions of types that can be ordered.
  * @tparam T Type of the expression.
  */
trait OrderingMethods[T] {this: Expression[T] =>

  /** Object that establishes the order of objects of this type. */
  implicit val orderingEvidence: Ordering[T]

  /***
    * Equals operation.
    * @param rhs Right-hand side of the equals operation.
    * @return Expression representing the Equals comparison between the two operands.
    */
  def ==(rhs: Expression[T]) = new RelationalExpression(this, Equals, rhs)

  /***
    * Not-equals operation.
    * @param rhs Right-hand side of the not-equals operation.
    * @return Expression representing the Not-equals comparison between the two operands.
    */
  def !=(rhs: Expression[T]) = new RelationalExpression(this, NotEquals, rhs)

  /***
    * Less-than operation.
    * @param rhs Right-hand side of the less-than operation.
    * @return Expression representing the Less-than comparison between the two operands.
    */
  def <(rhs: Expression[T]) = new RelationalExpression(this, LessThan, rhs)

  /***
    * Less-than-or-equals operation.
    * @param rhs Right-hand side of the Less-than-or-equals operation.
    * @return Expression representing the less-than-or-equals comparison between the two operands.
    */
  def <=(rhs: Expression[T]) = new RelationalExpression(this, LessEqualsThan, rhs)

  /***
    * Greater-than operation.
    * @param rhs Right-hand side of the Greater-than operation.
    * @return Expression representing the greater-than comparison between the two operands.
    */
  def >(rhs: Expression[T]) = new RelationalExpression(this, GreaterThan, rhs)

  /***
    * Greater-than-or-equals operation.
    * @param rhs Right-hand side of the Greater-than-or-equals operation.
    * @return Expression representing the greater-than-or-equals comparison between the two operands.
    */
  def >=(rhs: Expression[T]) = new RelationalExpression(this, GreaterEqualsThan, rhs)
}


/**
  * Trait containing methods for expressions of numeric types.
  * @tparam T Type of the expression.
  */
trait NumericMethods[T] {this: Expression[T] =>

  /** Object that implements arithmetic operations of a numeric type. */
  implicit val numericEvidence: Numeric[T]

  /**
    * Plus operation.
    * @param rhs Righ-hand side of the plus operation.
    * @return Expression representing the plus operation between the two operands.
    */
  def +(rhs: Expression[T])  = new ArithmeticExpression[T](this, Plus, rhs)

  /**
    * Minus operation.
    * @param rhs Righ-hand side of the minus operation.
    * @return Expression representing the minus operation between the two operands.
    */
  def -(rhs: Expression[T]) = new ArithmeticExpression[T](this, Minus, rhs)

  /**
    * Times operation.
    * @param rhs Righ-hand side of the times operation.
    * @return Expression representing the times operation between the two operands.
    */
  def *(rhs: Expression[T]) = new ArithmeticExpression[T](this, Times, rhs)

  /**
    * Divided-by operation.
    * @param rhs Righ-hand side of the divided-by operation.
    * @return Expression representing the divided-by operation between the two operands.
    */
  def /(rhs: Expression[T])   = new ArithmeticExpression[T](this, Div, rhs)
}


/**
  * Abstract class to be used by expression implementations. Forces all subclasses to provide a
  * TypeTag for the expression type.
  * @tparam T Type of the expression.
  */
abstract class AbstractExpr[T: TypeTag] extends Expression[T] {
  override val typeTagEvidence = implicitly[TypeTag[T]]
}

/**  Abstract class for boolean expressions. */
abstract class BooleanExpr extends AbstractExpr[Boolean] with BooleanMethods

/**
  * Abstract class for expressions of types that can be ordered. Forces subclasses to provide both
  * a TypeTag and a Ordering object.
  * @tparam T Type of the expression.
  */
abstract class OrderingExpr[T: Ordering: TypeTag] extends AbstractExpr[T] with OrderingMethods[T] {
  val orderingEvidence = implicitly[Ordering[T]]
}

/**
  * Abstract class for expressions of numeric types. Forces subclasses to provide a TypeTag,
  * a Ordering object, and a Numeric object.
  * @tparam T Type of the expression.
  */
abstract class NumericExpr[T: Numeric: Ordering: TypeTag] extends OrderingExpr[T] with NumericMethods[T] {
  val numericEvidence = implicitly[Numeric[T]]
}




