package com.evobootcamp.homeworks.basics

object Basics {
  def lcm(a: Int, b: Int): Int = (a, b) match {
    case (0, b) => b
    case (a, 0) => a
    case _ => a * b / gcd(a, b)
  }

  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (0, b) => b
    case (a, 0) => a
    case _ if (a > b) => gcd(a - b, b)
    case _ => gcd(a, b - a)
  }
}

object Test extends App {

  case class Person(firstName: String, lastName: String)

  object Person {
    def apply(name: String): Person = name.split(" ", 2).toList match {
      case firstName::lastName::Nil => new Person(firstName, lastName)
      case _                        => new Person(name, "")
    }
  }

//  val p = Person("Vlad Li")
//
//  p match {
//    case "Vlad" Person _  => println("Hello Vlad")
//  }

}

object Test2 extends App {
  case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
    def name: String = s"$firstName $lastName"
  }

  object Director {
    def older(d1: Director, d2: Director): Director =
      if (d1.yearOfBirth < d2.yearOfBirth) d1 else d2
  }

  case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) {
    def directorsAge: Int = yearOfRelease - director.yearOfBirth
    def isDirectedBy(d: Director): Boolean = d == director
  }

  object Film {
    def highestRating(f1: Film, f2: Film): Double =
        if (f1.imdbRating > f2.imdbRating) f1.imdbRating else f2.imdbRating
    def oldestDirectorAtTheTime(f1: Film, f2: Film): Director
      = if (f1.yearOfRelease - f1.director.yearOfBirth > f2.yearOfRelease - f2.director.yearOfBirth) f1.director
        else f2.director
  }
}
