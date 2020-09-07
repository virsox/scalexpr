package br.com.virsox.scalexpr

import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

/*** Tests for expression parsing */
class ExpressionParserTest extends AnyFlatSpec with Matchers {

  trait Fixture {
    val parser = ExpressionParser()
    val ctx1: Map[String, Any] = Map("value" -> 20.0, "sourceId" -> 1, "name" -> "Test")
    val ctx2: Map[String, Any] = Map("value" -> 7.2, "sourceId" -> 2)
  }


  def verify[T](expr: Try[Expression[T]], result: Expression[T]): Unit = {
    expr match {
      case Success(parsed) => parsed shouldBe result
      case Failure(ex) => fail(ex)
    }
  }

  "An ExpressionParser2" should "parse an int expression" in new Fixture {
    verify(parser.parseIntExpression("5 +3"), IntConstant(5) + IntConstant(3))
  }


  "An ExpressionParser" should "parse an int expression" in new Fixture {
    verify(parser.parseIntExpression("5 +3"), IntConstant(5) + IntConstant(3))
  }

  it should "consider operator precedence in an int expression" in new Fixture {
    verify(parser.parseIntExpression("4+1 * 2"), IntConstant(4) + (IntConstant(1) * IntConstant(2)))
  }

  it should "consider parenthesis precedence in an int expression" in new Fixture {
    verify(parser.parseIntExpression("3 * ((1 + 2) * (5 + 1))"),
      IntConstant(3) *
        ((IntConstant(1) + IntConstant(2)) *
         (IntConstant(5) + IntConstant(1))
        )
    )
  }

  it should "parse variables in an int expression" in new Fixture {
    verify(parser.parseIntExpression("sourceId  + 1 * 2"), IntVar("sourceId") + (IntConstant(1) * IntConstant(2)))
  }

  it should "resolve variables in an int expression with parenthesis" in new Fixture {
    verify(parser.parseIntExpression("3 * ((sourceId + 2) * (sourceId + 1))"),
      IntConstant(3) *
        ((IntVar("sourceId") + IntConstant(2)) *
         (IntVar("sourceId") + IntConstant(1))
        )
    )
  }

  it should "calculate a long expression" in new Fixture {
    verify(parser.parseLongExpression("2L + 30L"), LongConstant(2) + LongConstant(30))
  }

  it should "resolve variables in a long expression" in new Fixture {
    verify(parser.parseLongExpression("sourceId + 30L"), LongVar("sourceId") + LongConstant(30L))
  }


  it should "parse a double expression" in new Fixture {
    verify(parser.parseDoubleExpression("7.2 + 3.0"), DoubleConstant(7.2) + DoubleConstant(3.0))
  }

  it should "consider operator precedence in a double expression" in new Fixture {
    verify(parser.parseDoubleExpression("7.2 + 1.8*2.0"),
      DoubleConstant(7.2) +
        (DoubleConstant(1.8) * DoubleConstant(2.0))
    )
  }

  it should "consider parenthesis precedence in a double expression" in new Fixture {
    verify(parser.parseDoubleExpression("(7.2 + 1.8)*2.0"),
      (DoubleConstant(7.2) + DoubleConstant(1.8)) *
        DoubleConstant(2.0)
    )
  }

  it should "resolve variables in a double expression" in new Fixture {
    verify(parser.parseDoubleExpression("value + 3.0"), DoubleVar("value") + DoubleConstant(3.0))
  }

  it should "resolve multiple variables in a double expression with parenthesis" in new Fixture {
    verify(parser.parseDoubleExpression("(value +  3.0 ) *sourceId"),
      (DoubleVar("value") + DoubleConstant(3.0)) * DoubleVar("sourceId"))
  }


  "An ExpressionParser" should "parse equality expressions with Strings" in new Fixture {
    verify(parser.parseBooleanExpression("""name == "Wilson""""), StringVar("name") == StringConstant("Wilson"))
    verify(parser.parseBooleanExpression("""name != "John""""), StringVar("name") != StringConstant("John"))
  }

  it should "parse comparison expressions with Ints" in new Fixture {
    verify(parser.parseBooleanExpression("""age == 19"""), IntVar("age") == IntConstant(19))
    verify(parser.parseBooleanExpression("""age != 18"""), IntVar("age") != IntConstant(18))
    verify(parser.parseBooleanExpression("""age > 5"""),   IntVar("age")  > IntConstant(5))
    verify(parser.parseBooleanExpression("""age < 25"""),  IntVar("age")  < IntConstant(25))
    verify(parser.parseBooleanExpression("""age >= 19"""), IntVar("age") >= IntConstant(19))
    verify(parser.parseBooleanExpression("""age <= 19"""), IntVar("age") <= IntConstant(19))
  }

  it should "parse comparison expressions with Longs" in new Fixture {
    verify(parser.parseBooleanExpression("""id == 123456L"""), LongVar("id") == LongConstant(123456L))
    verify(parser.parseBooleanExpression("""id != 987654L"""), LongVar("id") != LongConstant(987654L))
  }

  it should "parse comparison expressions with Doubles" in new Fixture {

    verify(parser.parseBooleanExpression("""salary > 10000.00"""), DoubleVar("salary")  > DoubleConstant(10000.0))
    verify(parser.parseBooleanExpression("""average < 22.3"""),    DoubleVar("average") < DoubleConstant(22.3))
  }

  it should "correctly parse comparison expressions with Dates" in new Fixture {

    val date1 = Instant.parse("2015-12-01T10:00:00.000Z")
    val date2 = Instant.parse("2010-12-01T10:00:00.000Z")

    verify(parser.parseBooleanExpression("""start == 2015-12-01T10:00:00.000Z"""),
      DateTimeVar("start") == DateTimeConstant(date1))

    verify(parser.parseBooleanExpression("""end   < 2010-12-01T10:00:00.000Z"""),
      DateTimeVar("end") < DateTimeConstant(date2))
  }


  it should "parse composed boolean expressions" in new Fixture {
    verify(parser.parseBooleanExpression("""name == "Wilson" && age == 19"""),
      (StringVar("name") == StringConstant("Wilson")) && (IntVar("age") == IntConstant(19)))

    verify(parser.parseBooleanExpression("""name == "John" || age == 21"""),
      (StringVar("name") == StringConstant("John")) || (IntVar("age") == IntConstant(21)))
  }


  it should "parse boolean expressions with parenthesis" in new Fixture {
    verify(parser.parseBooleanExpression("""name == "Wilson" || (name == "Test" && age == 19)"""),
       (StringVar("name") == StringConstant("Wilson")) ||
       ((StringVar("name") == StringConstant("Test")) && (IntVar("age") == IntConstant(19))))
  }

  it should "parse complex boolean expressions with parenthesis" in new Fixture {
    verify(parser.parseBooleanExpression("""(name == "Wilson" && age  > (base + 2.0)) || (name == "Test" && age == ( 4 * 2))"""),
      (  (StringVar("name") == StringConstant("Wilson")) &&
         (DoubleVar("age") > (DoubleVar("base") + DoubleConstant(2.0)))) ||
      (  (StringVar("name") == StringConstant("Test")) &&
         (IntVar("age") == (IntConstant(4) * IntConstant(2))))
    )
  }
}

