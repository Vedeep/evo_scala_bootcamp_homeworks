package com.evobootcamp.homeworks.typeclass

// having two implementations for the same type (like different ways to make json out of User) is possible
// but considered to be bad

object TypeclassTask extends App {

  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  object Hashable {

    implicit class HashCodeSyntax[A](x: A) {
      def hash(implicit h: HashCode[A]): Int = h.hash(x)
    }

    implicit val stringHashable: HashCode[String] = entity => entity.hashCode()

  }

}
