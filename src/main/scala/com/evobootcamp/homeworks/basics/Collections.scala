package com.evobootcamp.homeworks.basics

import java.time.LocalDate
import java.time.chrono.IsoChronology
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ResolverStyle, SignStyle}
import java.time.temporal.ChronoField
import scala.annotation.tailrec

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = nums.foldLeft(Array(): Array[Int])((acc, num) => acc :+ acc.lastOption.getOrElse(0) + num)

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = nums.take(n).zip(nums.drop(n)).map({ case (a, b) => Array(a, b) }).flatten

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = for {
    c   <- candies
    max = candies.max
  } yield c + extraCandies >= max

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    points.map(_.head).sorted match {
      case p: Array[Int] if p.length >= 2  => p.zip(p.tail).map({ case (a, b) => b - a }).max
      case _  => 0
    }
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    "([\\(\\)])".r.findAllIn(s).toList.foldLeft((0, 0))((acc, v) => v match {
      case "("  => (acc._1 + 1, acc._2)
      case ")"  => (acc._1 - 1, if (acc._1 > acc._2) acc._1 else acc._2)
    })._2
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    s.split("").foldLeft((0, 0))((acc, v) => {
      acc match {
        case (l, r) => v match {
          case "R"  => (l + 1, if (l + 1 == 0) r + 1 else r)
          case "L"  => (l - 1, if (l - 1 == 0) r + 1 else r)
        }
      }
    })._2
  }

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val matZipped = mat.zipWithIndex
    matZipped.map {
      case (mt1, i1) => {
        val iRange1 = i1 - K to i1 + K

        mt1.zipWithIndex.map({
          case (_, i2) => {
            val iRange2 = i2 - K to i2 + K

            matZipped.filter({
              case (_, idx) =>  iRange1 contains idx
            }).flatMap({
              case (m2, _) => {
                m2.zipWithIndex.filter({
                  case (_, idx) => iRange2 contains idx
                }).map {
                  case (v, _) => v
                }
              }
            }).sum
          }
        })
      }
    }
  }

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    @tailrec
    def recursive(list: List[T], result: List[T], zero: T): List[T] = list match {
      case Nil  => result
      case x :: xs  => {
        val a = f(zero, x)
        recursive(xs, result.appended(a), a)
      }
    }

    recursive(list, List(zero), zero)
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    @tailrec
    def recursive(result: List[(Char, Int)], list: List[String], char: String, count: Int): List[(Char, Int)] = list match {
      case Nil  => if (count > 0) result.appended((char.charAt(0), count)) else result
      case x :: xs  => {
        recursive(
          if (char != x && count > 0) result.appended((char.charAt(0), count)) else result,
          xs,
          x,
          if (char != x) 1 else count + 1
        )
      }
    }

    recursive(List(), s.split("").toList, "", 0)
  }

  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = map.groupMap({
    case (_, c) => c
  })({
    case (s, _) => s
  }).toList.map({
    case (i, v) => (v.toSet, i)
  })

  object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      var res = Array[Int]()
      val nums1 = nums.zipWithIndex
      val nums2 = nums1.drop(1)

      nums1.forall(n1 => {
        nums2.forall(n2 => {
          if (n1._2 != n2._2 && n1._1 + n2._1 == target) {
            res = res.appendedAll(Array(n1._2, n2._2))
            false
          } else {
            true
          }
        })

        if (res.length == 0) {
          true
        } else {
          false
        }
      })

      res
    }
  }

  def main(args: Array[String]): Unit = {
    val format = new DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4)
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .appendValue(ChronoField.DAY_OF_MONTH, 2)
      .toFormatter()

    println(LocalDate.parse("20190215", format))
  }
}
