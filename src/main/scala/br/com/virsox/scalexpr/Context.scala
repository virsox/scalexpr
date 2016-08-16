package br.com.virsox.scalexpr

import scala.reflect.runtime.universe._

/*** Encapsulates the ContextLike type class. */
object Context {

  /**
    * Trait that defines the behaviour implemented by classes that can be used as context of variable resolution.
    * Objects of the class T must associate values to named fields. These values can be instances of primitive types
    * or nested instances of the class T that also contain named fields.
    *
    * @tparam T Type that implements the type class behaviour.
    */
  trait ContextLike[T] {

    /***
      * Obtain from the context a value of type B that is associated with a named field.
      * @param ctx Context from which the value is obtained.
      * @param field Name of the field.
      * @tparam B Type of the value.
      * @return An option containing the value if the field exists, None otherwise.
      */
    def get[B: TypeTag](ctx: T, field: String): Option[B]

    /***
      * Obtain from the context a nested instance of type T that is associated with a named field.
      * @param ctx Context from which the instance is obtained.
      * @param field Name of the field.
      * @return An option containing the instance if the field exists, None otherwise.
      */
    def nested(ctx: T, field: String): Option[T]
  }

  /** Empty context. */
  object EmptyContext

  /** Encapsulates implicit implementations of `ContextLike`. */
  object ContextLike {

    /** ContextLike implementation for an empty context. */
    implicit object ContextLikeEmpty extends ContextLike[EmptyContext.type] {
      override def get[B: TypeTag](ctx: EmptyContext.type, field: String): Option[B] = None
      override def nested(ctx: EmptyContext.type, field: String): Option[EmptyContext.type] = None
    }

    /** ContextLike implementation for maps that have String keys. */
    implicit object ContextLikeMap extends ContextLike[Map[String, _]] {
      override def get[B: TypeTag](ctx: Map[String, _], field: String): Option[B] =
        ctx.get(field).map(_.asInstanceOf[B])
      override def nested(ctx: Map[String, _], field: String): Option[Map[String, _]] =
        ctx.get(field).map(_.asInstanceOf[Map[String, _]])
    }
  }


}
