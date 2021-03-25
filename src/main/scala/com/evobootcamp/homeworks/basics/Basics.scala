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
