package com.evobootcamp.homeworks.basics

object Basics {
  def lcd(a: Int, b: Int): Int = if (b == 0) a else if (a == 0) b else a * b / gcd(a, b)

  def gcd(a: Int, b: Int): Int = {
    if (a == 0) {
      b
    } else if (b == 0) {
      a
    } else if (a > b) {
      gcd(a - b, b)
    } else if (a < b) {
      gcd(a, b - a)
    } else {
      a
    }
  }
}
