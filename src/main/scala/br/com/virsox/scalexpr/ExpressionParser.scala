package br.com.virsox.scalexpr

import java.time.Instant

import fastparse._

import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/** Companion object. */
object ExpressionParser {

  /***
    * Defines a function with a name.
    * @param f Function definition.
    * @param name Name of the function.
    * @tparam T Type of the function parameter.
    * @tparam V Type of the function return type.
    */
  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T) = f(t)
    override def toString() = name

  }

  def apply() = new ExpressionParser()
  case class ExpressionParsingException(msg: String) extends Exception
}


/***
  * Parser of expressions.
  */
class ExpressionParser {

  import ExpressionParser._

  /***
    * Parse a String as a boolean expression.
    * @param str String to be parsed.
    * @return Boolean expression if the String can be successfully parsed, a Failure otherwise.
    */
  def parseBooleanExpression(str: String): Try[Expression[Boolean]] = booleanExpr.parse(str) match {
    case Parsed.Success(value, _) => Success(value)
    case f: Parsed.Failure => { //(_, _, _)  => {
      println(s"[${f.index}], ${f.extra.traced.trace}")
      Failure(ExpressionParsingException("Error"))
    }
  }

  /***
    * Parse a String as a double expression.
    * @param str String to be parsed.
    * @return Double expression if the String can be successfully parsed, a Failure otherwise.
    */

  def parseDoubleExpression(str: String): Try[Expression[Double]] = expr[Double].parse(str) match {
    case Parsed.Success(value, _) => Success(value)
    case Parsed.Failure(_, _, _)  => Failure(ExpressionParsingException("Error"))
  }

  /***
    * Parse a String as a int expression.
    * @param str String to be parsed.
    * @return Int expression if the String can be successfully parsed, a Failure otherwise.
    */
  def parseIntExpression(str: String): Try[Expression[Int]] = expr[Int].parse(str) match {
    case Parsed.Success(value, _) => Success(value)
    case Parsed.Failure(_, _, _)  => Failure(ExpressionParsingException("Error"))
  }

  /***
    * Parse a String as a long expression.
    * @param str String to be parsed.
    * @return Long expression if the String can be successfully parsed, a Failure otherwise.
    */
  def parseLongExpression(str: String): Try[Expression[Long]] = expr[Long].parse(str) match {
    case Parsed.Success(value, _) => Success(value)
    case Parsed.Failure(_, _, _)  => Failure(ExpressionParsingException("Error"))
  }


  // ---------------------------------------------------------------
  // -----------------    General parsers     ----------------------
  // ---------------------------------------------------------------
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val space      = P(CharsWhile(Whitespace).?)
  val chars      = P(CharIn(('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')))
  val digits     = P(CharIn('0' to '9'))


  // ---------------------------------------------------------------
  // -----------------    Number parsers      ----------------------
  // ---------------------------------------------------------------

  // a decimal is 0 or [+-] followed by [1-9] followed by digits
  val decimalParser = P("0" | CharIn("+-").? ~ CharIn('1' to '9') ~ digits.rep)

  // an int is a decimal number that does not end with a L
  val intParser = P(decimalParser ~ !CharIn("Ll.")).!

  // a long is a decimal number that ends with a L character
  val longParser = P(decimalParser.! ~ CharIn("Ll").!).map(_._1)

  // a double is a fractional number
  val doubleParser = P(decimalParser ~ "." ~ digits.rep).!

  // convert the parsed numbers to constant objects
  val intLiteral:    P[NumericExpr[Int]]    = intParser.map(s => IntConstant(s.toInt))
  val longLiteral:   P[NumericExpr[Long]]   = longParser.map(s => LongConstant(s.toLong))
  val doubleLiteral: P[NumericExpr[Double]] = doubleParser.map(s => DoubleConstant(s.toDouble))


  // ---------------------------------------------------------------
  // -----------------    Variable parsers    ----------------------
  // ---------------------------------------------------------------

  // a valid identifier is composed of a character, followed by n characters or digits
  val identifierParser  = P(chars ~ (chars | digits).rep)

  // a variable is a sequence of identifiers separated by "."
  val variableParser    = P(identifierParser ~ ("." ~ identifierParser).rep)

  val intVariable      = variableParser.!.map(IntVar(_))
  val longVariable     = variableParser.!.map(LongVar(_))
  val doubleVariable   = variableParser.!.map(DoubleVar(_))
  val dateTimeVariable = variableParser.!.map(DateTimeVar(_))
  val booleanVariable  = variableParser.!.map(BooleanVar(_))
  val stringVariable   = variableParser.!.map(StringVar(_))


  // ---------------------------------------------------------------
  // --------------    Date literal parsers    ---------------------
  // ---------------------------------------------------------------
  val yearParser  = P(digits.rep(min = 4, max = 4))
  val monthParser = P(StringIn((1 to 12).map(_.formatted("%02d")):_*))
  val dayParser   = P(StringIn((1 to 31).map(_.formatted("%02d")):_*))

  // months with 31 days
  val months31 = Seq(1, 3, 5, 7, 8, 10, 12)

  // months with 30 days
  val months30 = Seq(4, 6, 9, 11)

  // parses a date and validates it
  val dateParser = P(yearParser.! ~ "-" ~ monthParser.! ~ "-" ~ dayParser.!).
    map { case (year, month, day) =>  (year.toInt, month.toInt, day.toInt) }.
    filter { case (year, month, day) => {
      if (months31.contains(month) && day > 31) false
      else if (months30.contains(month) && day > 30) false
      else if ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0)) && day > 29) false // leap year
      else if (day > 28) false
      true
    }}.!

  val hourParser = P(StringIn((0 to 23).map(_.formatted("%02d")):_*))
  val minSecondParser = P(StringIn((0 to 59).map(_.formatted("%02d")):_*))
  val milliParser = P(digits.rep(min = 3, max = 3))
  val timeParser = P(hourParser ~ ":" ~ minSecondParser ~ ":" ~ minSecondParser ~ "." ~ milliParser)
  val dateTimeParser = P(dateParser ~ "T" ~ timeParser ~ "Z").!
  val dateTimeLiteral = dateTimeParser.map((s) => DateTimeConstant(Instant.parse(s)))


  // ---------------------------------------------------------------
  // ------------------   String parsers    ------------------------
  // ---------------------------------------------------------------
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  val strChars = P(CharsWhile(StringChars))
  val stringLiteral  = P("\"" ~ strChars.! ~ "\"").map(StringConstant(_))



  // ---------------------------------------------------------------
  // -----------------   Numeric expressions   ---------------------
  // ---------------------------------------------------------------
  /***
    * Obtains a numeric variable parser based on the informed type.
    * @tparam T Type of the variable returned by the parser.
    * @return numeric variable parser.
    */
  def numericVariable[T: TypeTag]: P[NumericExpr[T]] = {
    val tt = implicitly[TypeTag[T]]
    tt.tpe match {
      case int    if typeOf[T] =:= typeOf[Int]    => intVariable.asInstanceOf[P[NumericExpr[T]]]
      case long   if typeOf[T] =:= typeOf[Long]   => longVariable.asInstanceOf[P[NumericExpr[T]]]
      case double if typeOf[T] =:= typeOf[Double] => doubleVariable.asInstanceOf[P[NumericExpr[T]]]
    }
  }

  /***
    * Obtains a numeric literal parser based on the informed type.
    * @tparam T Type of the literal returned by the parser.
    * @return numeric literal parser.
    */
  def numericLiteral[T: TypeTag]: P[NumericExpr[T]] = {
    val tt = implicitly[TypeTag[T]]
    tt.tpe match {
      case int    if typeOf[T] =:= typeOf[Int]    => intLiteral.asInstanceOf[P[NumericExpr[T]]]
      case long   if typeOf[T] =:= typeOf[Long]   => longLiteral.asInstanceOf[P[NumericExpr[T]]]
      case double if typeOf[T] =:= typeOf[Double] => doubleLiteral.asInstanceOf[P[NumericExpr[T]]]
    }
  }

  /***
    * Builds a ArithmeticExpression composed of a left-hand subexpression followed by a sequence of
    * operations performed on this subexpression.
    *
    * @param tree Tuple containing the left-hand subexpression and the sequence of operations.
    * @tparam T Type of the numeric expression.
    * @return ArithmeticExpression representing the operations.
    */
  def evalNumeric[T: TypeTag](tree: (NumericExpr[T], Seq[(String, NumericExpr[T])])): NumericExpr[T] = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
    }}
  }

  /** Parser of a numeric expression with parenthesis. */
  def parens[T: TypeTag]: P[NumericExpr[T]] = P(space ~ "(" ~ expr[T] ~ ")")

  /** Parser of multiplication / division factors. */
  def factor[T: TypeTag]: P[NumericExpr[T]] = P(space ~ (numericLiteral[T] | numericVariable[T] | parens))

  /** Parser for division or multiplication expressions. */
  def divMul[T: TypeTag]: P[NumericExpr[T]] = P(factor[T] ~ space ~ (CharIn("*/").! ~ factor[T]).rep).
    map(evalNumeric[T])

  /** Parser for arithmetic expressions. */
  def expr[T: TypeTag]: P[NumericExpr[T]]   = P(divMul[T] ~ space ~ (CharIn("+-").! ~ divMul[T]).rep).
    map(evalNumeric[T])


  // ---------------------------------------------------------------
  // -----------------   Relational expressions   ------------------
  // ---------------------------------------------------------------

  /***
    * Builds a RelationalExpression composed of a left-hand subexpression followed by a sequence of
    * operations performed on this subexpression.
    *
    * @param tree Tuple containing the left-hand subexpression and the sequence of operations.
    * @tparam T Type of the numeric expression.
    * @return RelationalExpression representing the operations.
    */
  def evalRelational[T: Ordering](tree: (OrderingExpr[T], String, OrderingExpr[T])): RelationalExpression[T] = {
    val (left, op, right) = tree
    op match {
      case "==" => left == right
      case "!=" => left != right
      case ">=" => left >= right
      case "<=" => left <= right
      case ">"  => left > right
      case "<"  => left < right
    }
  }


  val dateTimeTerm = P(space ~ (dateTimeVariable | dateTimeLiteral))
  val stringTerm = P(space ~ (stringLiteral | stringVariable))

  val relationalOperators = P(StringIn("==", "!=", "<=", ">=", "<", ">"))

  // both sides need to be of the same type - there is no conversion
  val strRelationalExpr    = P(stringTerm   ~ space ~ relationalOperators.! ~ stringTerm).map(evalRelational[String])
  val intRelationalExpr    = P(expr[Int]    ~ space ~ relationalOperators.! ~ expr[Int]).map(evalRelational[Int])
  val longRelationalExpr   = P(expr[Long]   ~ space ~ relationalOperators.! ~ expr[Long]).map(evalRelational[Long])
  val doubleRelationalExpr = P(expr[Double] ~ space ~ relationalOperators.! ~ expr[Double]).map(evalRelational[Double])
  val dateTimeRelationalExpr = P(dateTimeTerm ~ space ~ relationalOperators.! ~ dateTimeTerm).
    map(evalRelational[Instant])
  val relationalExpr = P(strRelationalExpr | dateTimeRelationalExpr | intRelationalExpr | longRelationalExpr |
    doubleRelationalExpr)

  // ---------------------------------------------------------------
  // ------------------   Boolean expressions   --------------------
  // ---------------------------------------------------------------

  val booleanLiteral = P("true" | "false").!.map((s) => BooleanConstant(s.toBoolean))
  val booleanParens: P[BooleanExpr] = P("(" ~/ booleanExpr ~ ")")

  val booleanTerm = P(space ~ (booleanParens | booleanLiteral | relationalExpr))

  val booleanOperators = P(StringIn("&&", "||"))
  val booleanExpr = P(booleanTerm ~ (space ~ booleanOperators.! ~ booleanTerm).rep).map((s) => {
    val (base, ops) = s
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "&&" => left && right
      case "||" => left || right
    }}
  })

}

