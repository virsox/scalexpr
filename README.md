# scalexpr
Scalexpr is a expression parser and evaluator for Scala. The library has been created to provide a very
simple way to parse expression Strings and evaluate them in a type-safe manner.

## Build
A [sbt](http://www.scala-sbt.org) script is provided and can be used to build _scalexpr_. The only
dependency is the incredible [fastparse](https://github.com/lihaoyi/fastparse) library.



## Usage
On its simplest form, _scalexp_ can be used as:
 
 ```scala
 import br.com.virsox.scalexpr._
 import scala.util.{Failure, Success}
 
 val parser = ExpressionParser()
 parser.parseIntExpression("10 + 5") match {
  case Success(expr) => println(expr.resolve()); assert(expr.resolve() == 15)
  case Failure(ex)   => println(ex)
 }
 ```
 
The library also support more complex expressions containing relational and boolean operators:
  ```scala
  val expr1 = parser.parseBooleanExpression("(10 + 5 > 12) && (4 - 3 <= 1)").get
  println(expr1.resolve())     // prints true
  ```
    
And the use of variables:
 ```scala
  val ctx2: Map[String, Any] = Map("age" -> 19, "salary" -> 32000)
  val expr2 = parser.parseBooleanExpression("age == 19 && salary > 40000 ").get
  println(expr2.resolve(ctx2))  // prints false
  ```
In this case, `age` and `salary` are parsed as variables that are resolved in the `resolve` method based on the 
content of the context map. 
    
    
 Finally, the library also supports other data types, including `Long`, `Double`, `String`, and
 `java.time.Instant`:
 ```scala
  import java.time.Instant
 
 val ts = Instant.parse("2015-10-01T12:00:00.000Z")
 val ctx3 = Map("id" -> 10L, "name" -> "sensor1", "timestamp" -> ts)
 val expr3 = parser.parseBooleanExpression(
    """(id == 10L || name == "sensor1") && timestamp > 2012-01-01T00:00:00.000Z""").get
 println(expr3.resolve(ctx3))  // prints true
 ```
 
## Advanced Usage 
 
### Context implementation
The library provides a `ContextLike` type class that must be implemented by classes whose objects
can be used as context when resolving expressions:
 ```scala
 trait ContextLike[T] {
   def get[B: TypeTag](ctx: T, field: String): Option[B]
   def nested(ctx: T, field: String): Option[T]
 }
```

Objects of this type class are passed implictly to the `resolve` method and are used to obtain values 
based on the variable names. _scalexpr_ provides a default implementation for `Map`s with `String` keys, but
other implementations can be easily created.
  
### Expression representation
_scalexpr_ provides a DSL that can be used to create `Expression` objects independently of the
parsing functionalities. The following snippet shows how the DSL can be used:

```scala
import br.com.virsox.scalexpr._

val expr1 = IntConstant(10) + IntConstant(2) * IntConstant(3)
println(expr1.resolve())          // prints 16

val expr2 = (DoubleConstant(6.0) / DoubleConstant(2.0)) > DoubleConstant(1.0)
println(expr2.resolve())          // prints true

val ctx3 = Map("name" -> "John", "age" -> 9)
val expr3 = (StringVar("name") == StringConstant("John")) && (IntVar("age") > IntConstant(10))
println(expr3.resolve(ctx3))     // prints false
```

## Contributions
The library has been designed to be as straightforward to use as possible and, right now, it already implements
most functionalities originally planned. Nevertheless, the following functionalities are still missing:

  * Automatic conversion between numeric types
  * String functions (toUpperCase, toLowerCase, trim)
  * Alternative date representations

Contributions are welcome!






