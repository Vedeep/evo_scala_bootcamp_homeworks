package com.evobootcamp.homeworks.basics

import Collections._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CollectionsSpec extends AnyFlatSpec {
  "runningSum" should "return the running sum of nums" in {
    runningSum(Array(1, 2, 3, 4)) shouldEqual Array(1, 3, 6, 10)
    runningSum(Array(1, 1, 1, 1, 1)) shouldEqual Array(1, 2, 3, 4, 5)
    runningSum(Array(3, 1, 2, 10, 1)) shouldEqual Array(3, 4, 6, 16, 17)
  }

  "shuffle" should "given the array nums consisting of 2n elements" in {
    shuffle(Array(2, 5, 1, 3, 4, 7), 3) shouldEqual Array(2, 3, 5, 4, 1, 7)
    shuffle(Array(1, 2, 3, 4, 4, 3, 2, 1), 4) shouldEqual Array(1, 4, 2, 3, 3, 2, 4, 1)
    shuffle(Array(1, 1, 2, 2), 2) shouldEqual Array(1, 2, 1, 2)
  }

  "maximumWealth" should "return maximum sum of subarrays" in {
    maximumWealth(Array(Array(1, 2, 3), Array(3, 2, 1))) shouldEqual 6
    maximumWealth(Array(Array(1, 5), Array(7, 3), Array(3, 5))) shouldEqual 10
    maximumWealth(Array(Array(2, 8, 7), Array(7, 1, 3), Array(1, 9, 5))) shouldEqual 17
  }

  "kidsWithCandies" should "return an array of boolean with greatest possible number of candies" in {
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
    kidsWithCandies(Array(4, 2, 1, 1, 2), 1) shouldEqual Array(true, false, false, false, false)
    kidsWithCandies(Array(12, 1, 12), 0) shouldEqual Array(true, false, true)
  }

  "maxWidthOfVerticalArea" should "return widest vertical area" in {
    maxWidthOfVerticalArea(Array(Array(8, 7), Array(9, 9), Array(7, 4), Array(9, 7))) shouldEqual 1
    maxWidthOfVerticalArea(Array(Array(3, 1), Array(9, 0), Array(1, 0), Array(1, 4), Array(5, 3), Array(8, 8))) shouldEqual 3
  }

  "maxDepth" should "return maximum nesting depth of the parentheses" in {
    maxDepth("(1+(2*3)+((8)/4))+1") shouldEqual 3
    maxDepth("(1)+((2))+(((3)))") shouldEqual 3
    maxDepth("1+(2*3)/(2-1)") shouldEqual 1
    maxDepth("1") shouldEqual 0
  }

  "balancedStringSplit" should "return the maximum amount of split balanced strings" in {
    balancedStringSplit("RLRRLLRLRL") shouldEqual 4
    balancedStringSplit("RLLLLRRRLR") shouldEqual 3
    balancedStringSplit("LLLLRRRR") shouldEqual 1
    balancedStringSplit("RLRRRLLRLL") shouldEqual 2
  }

  "matrixBlockSum" should "return sum of matrix" in {
    matrixBlockSum(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 1)
      .shouldEqual(Array(Array(12, 21, 16), Array(27, 45, 33), Array(24, 39, 28)))

    matrixBlockSum(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 2)
      .shouldEqual(Array(Array(45, 45, 45), Array(45, 45, 45), Array(45, 45, 45)))
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  "count" should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }

  "sort considering equal values" should "be correct on example 1" in {
    val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    val obtained = sortConsideringEqualValues(input)
    obtained shouldEqual expected
  }

  it should "be correct on example 2" in {
    val values = Set("a1", "a2", "b1", "c1", "c2", "d1").map { x =>
      x -> x.head.toInt
    }.toMap

    sortConsideringEqualValues(values) shouldEqual List(
      Set("a1", "a2") -> 'a'.toInt,
      Set("b1") -> 'b'.toInt,
      Set("c1", "c2") -> 'c'.toInt,
      Set("d1") -> 'd'.toInt,
    )
  }
}
