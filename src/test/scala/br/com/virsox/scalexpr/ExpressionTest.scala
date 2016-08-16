package br.com.virsox.scalexpr

import java.time.Instant

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/*** Tests for expression evaluation */
@RunWith(classOf[JUnitRunner])
class ExpressionTest extends FlatSpec
  with Matchers {

  trait BooleanConstants {
    val consTrue1  = BooleanConstant(true)
    val consTrue2  = BooleanConstant(true)
    val consFalse1 = BooleanConstant(false)
    val consFalse2 = BooleanConstant(false)
  }

  "A logical expression" should "evaluate the && operator" in new BooleanConstants {
    (consFalse1 && consFalse2).resolve() shouldBe false
    (consFalse1 && consTrue1 ).resolve() shouldBe false
    (consTrue1  && consFalse2).resolve() shouldBe false
    (consTrue1  && consTrue2 ).resolve() shouldBe true
  }

  it should "evaluate the || operator" in new BooleanConstants {
    (consFalse1 || consFalse2).resolve() shouldBe false
    (consFalse1 || consTrue1 ).resolve() shouldBe true
    (consTrue1  || consFalse2).resolve() shouldBe true
    (consTrue1  || consTrue2 ).resolve() shouldBe true
  }

  // ---------------------------------------------------------------------------------------------------

  trait IntConstants {
    val cons1 = IntConstant(10)
    val cons2 = IntConstant(20)
    val cons3 = IntConstant(10)
  }

  "A relational expression" should "evaluate the == operator" in new IntConstants {
    (cons1 == cons2).resolve() shouldBe false
    (cons2 == cons1).resolve() shouldBe false
    (cons1 == cons3).resolve() shouldBe true
    (cons3 == cons1).resolve() shouldBe true
  }

  it should "evaluate the != operator" in new IntConstants {
    (cons1 != cons2).resolve() shouldBe true
    (cons2 != cons1).resolve() shouldBe true
    (cons1 != cons3).resolve() shouldBe false
    (cons3 != cons1).resolve() shouldBe false
  }

  it should "evaluate the < operator" in new IntConstants {
    (cons1 < cons2).resolve() shouldBe true
    (cons2 < cons1).resolve() shouldBe false
    (cons1 < cons3).resolve() shouldBe false
    (cons3 < cons1).resolve() shouldBe false
  }

  it should "evaluate the <= operator" in new IntConstants {
    (cons1 <= cons2).resolve() shouldBe true
    (cons2 <= cons1).resolve() shouldBe false
    (cons1 <= cons3).resolve() shouldBe true
    (cons3 <= cons1).resolve() shouldBe true
  }

  it should "evaluate the > operator" in new IntConstants {
    (cons1 > cons2).resolve() shouldBe false
    (cons2 > cons1).resolve() shouldBe true
    (cons1 > cons3).resolve() shouldBe false
    (cons3 > cons1).resolve() shouldBe false
  }

  it should "evaluate the >= operator" in new IntConstants {
    (cons1 >= cons2).resolve() shouldBe false
    (cons2 >= cons1).resolve() shouldBe true
    (cons1 >= cons3).resolve() shouldBe true
    (cons3 >= cons1).resolve() shouldBe true
  }

  // ---------------------------------------------------------------------------------------------------

  "An arithmetric expression" should "evaluate arithmetic operators" in {
    val cons1 = IntConstant(20)
    val cons2 = IntConstant(10)

    (cons1 + cons2).resolve() shouldBe 30
    (cons1 - cons2).resolve() shouldBe 10
    (cons1 * cons2).resolve() shouldBe 200
    (cons1 / cons2).resolve() shouldBe 2
  }

  it should "evaluate sub expressions" in {
    val cons1 = IntConstant(20)
    val cons2 = IntConstant(10)
    val cons3 = IntConstant(2)

    ((cons1 + cons2) * cons3).resolve() shouldBe 60
    ((cons1 - cons2) / cons3).resolve() shouldBe 5
  }

  // ---------------------------------------------------------------------------------------------------

  "A complex logical expression" should "evaluate relational sub expressions" in {
    val cons1 = IntConstant(10)
    val cons2 = IntConstant(20)

    ((cons1 == cons2) || (cons1 > cons2)).resolve() shouldBe false
    ((cons1 != cons2) || (cons1 > cons2)).resolve() shouldBe true
    ((cons1 == cons2) || (cons1 <= cons2)).resolve() shouldBe true
    ((cons1 != cons2) && (cons1 <= cons2)).resolve() shouldBe true
  }

  // ---------------------------------------------------------------------------------------------------

  "A variable" should "resolve to the correct value" in {
    val var1 = IntVar("age")
    val var2 = StringVar("name")

    val context = Map("age" -> 19, "name" -> "John")
    var1.resolve(context) shouldBe 19
    var2.resolve(context) shouldBe "John"

  }

  it should "be used in arithmetic expressions" in {
    val var1 = IntVar("age")
    val value = IntConstant(10)

    val context = Map("age" -> 19, "name" -> "John")
    (var1 + value).resolve(context) shouldBe 29
  }

  it should "be used in arithmetic expressions with other variables" in {
    val var1 = IntVar("attr1")
    val var2 = IntVar("attr2")

    val context: Map[String, Any] = Map("attr1" -> 10, "attr2" -> 30)
    (var1 + var2).resolve(context) shouldBe 40
  }


  it should "be used in relational expressions" in {
    val var1 = IntVar("age")
    val value = IntConstant(10)

    val context: Map[String, Any] = Map("age" -> 19)
    (var1 > value).resolve(context) shouldBe true
    (var1 < value).resolve(context) shouldBe false
  }

  // ---------------------------------------------------------------------------------------------------


  trait TestWithDates {
    val date1 = Instant.parse("2015-12-01T10:00:00.000Z")
    val date2 = Instant.parse("2010-10-10T10:00:00.000Z")

  }

  "A relational expression" should "evaluate when using dates" in new TestWithDates {

    val cons1 = DateTimeConstant(date1)
    val cons2 = DateTimeConstant(date2)

    (cons1 > cons2).resolve() shouldBe true
    (cons1 < cons2).resolve() shouldBe false
    (cons2 < cons1).resolve() shouldBe true
    (cons2 > cons1).resolve() shouldBe false
  }

  it should "evaluate when using date variables" in new TestWithDates {

    val cons1 = DateTimeConstant(date1)
    val var1 = DateTimeVar("since")

    val context: Map[String, Any] = Map("since" -> "2010-10-10T10:00:00.000Z")

    (cons1 > var1).resolve(context) shouldBe true
    (cons1 < var1).resolve(context) shouldBe false
    (var1 < cons1).resolve(context) shouldBe true
    (var1 > cons1).resolve(context) shouldBe false
  }


}
