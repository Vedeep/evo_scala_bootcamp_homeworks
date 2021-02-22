package com.evobootcamp.homeworks.typeclass

// make as many exercises as you can

object Task1 {
  final case class Money(amount: BigDecimal)
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 extends App {

  trait Show[T] {
    def show(entity: T): String
  }

  object Show {
    def apply[F](implicit instance: Show[F]): Show[F] = instance
  }

  final case class User(id: String, name: String)

  implicit class ShowSyntax[A](x: A) {
    def show(implicit s: Show[A]): String = s.show(x)
  }

  object Showable {
    implicit val userShow: Show[User] = entity => (entity.id, entity.name).toString()
  }

}

object Task3 extends App {
  type Error = String
  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }
  
  object Parse {
    def apply[F: Parse]: Parse[F] = implicitly[Parse[F]]
  }

  implicit class ParseSyntax(forType: String) {
    def parse[T: Parse]: Either[Error, T] = Parse[T] parse forType
  }

  object Parsable {
    implicit val stringToUser: Parse[User] = entity => entity.split(',').toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _ => Left("Not parsable")
    }
  }

  final case class User(id: String, name: String)
}


object Task4 extends App {

  trait Equals[T] {
    def ===(entity: T): Boolean
  }

  implicit class EqualsSyntax[A](fromEntity: A) {
    def ===(toEntity: A): Boolean = fromEntity equals toEntity
  }

}

object AdvancedHomework extends App {

  trait MyFlatMap[T] {
    def myFlatMap(f: T => IterableOnce[T]): T
  }

  implicit class MyFlatMapSyntax[A](entity: Iterable[A]) {
    def myFlatMap(f: A => IterableOnce[A]): Iterable[A] = entity.map(f).flatten
  }
  
}
