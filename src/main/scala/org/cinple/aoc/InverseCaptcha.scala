package org.cinple.aoc

// The captcha requires you to review a sequence of digits (your puzzle inverse-captcha-input.dat) and find the sum
// of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is
// the first digit in the list.
// 
// For example:
// 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
// 1111 produces 4 because each digit (all 1) matches the next.
// 1234 produces 0 because no digit matches the next.
// 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
// What is the solution to your captcha?

import scala.io.Source.fromInputStream

object InverseCaptcha {
  private val inputFn = "/inverse-captcha-input.dat"

  private def addOnEq(digits: Array[Int], part2: Boolean = false): Int = {
    val len = digits.length
    val stepLen = if (part2) len / 2 else 1
    digits.zipWithIndex.foldLeft(0) { case (acc, (d, i)) =>
      val ii = if (i + stepLen >= len) i + stepLen - len else i + stepLen
      if (d == digits(ii)) acc + d
      else acc
    }
  }

  private def part1(): Unit = {
    assert(addOnEq(Array(1, 1, 2, 2)) == 3)
    assert(addOnEq(Array(1, 1, 1, 1)) == 4)
    assert(addOnEq(Array(1, 2, 3, 4)) == 0)
    assert(addOnEq(Array(9, 1, 2, 1, 2, 1, 2, 9)) == 9)

    val stream = getClass.getResourceAsStream(inputFn)
    val digits = fromInputStream(stream).map(_.asDigit).toArray
    println(addOnEq(digits))
  }

  private def part2(): Unit = {
    assert(addOnEq(Array(1, 2, 1, 2), part2 = true) == 6)
    assert(addOnEq(Array(1, 2, 2, 1), part2 = true) == 0)
    assert(addOnEq(Array(1, 2, 3, 4, 2, 5), part2 = true) == 4)
    assert(addOnEq(Array(1, 2, 3, 1, 2, 3), part2 = true) == 12)
    assert(addOnEq(Array(1, 2, 1, 3, 1, 4, 1, 5), part2 = true) == 4)

    val stream = getClass.getResourceAsStream(inputFn)
    val digits = fromInputStream(stream).map(_.asDigit).toArray
    println(addOnEq(digits, part2 = true))
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
