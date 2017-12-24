package org.cinple.aoc

/**
  * --- Day 1: Inverse Captcha ---
The night before Christmas, one of Santa's Elves calls you in a panic. "The printer's broken! We can't print the Naughty or Nice List!" By the time you make it to sub-basement 17, there are only a few minutes until midnight. "We have a big problem," she says; "there must be almost fifty bugs in this system, but nothing else can print The List. Stand in this square, quick! There's no time to explain; if you can convince them to pay you in stars, you'll be able to--" She pulls a lever and the world goes blurry.

When your eyes can focus again, everything seems a lot more pixelated than before. She must have sent you inside the computer! You check the system clock: 25 milliseconds until midnight. With that much time, you should be able to collect all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day millisecond in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You're standing in a room with "digitization quarantine" written in LEDs along one wall. The only door is locked, but it includes a small interface. "Restricted Area - Strictly No Digitized Users Allowed."

It goes on to explain that you may only leave by solving a captcha to prove you're not a human. Apparently, you only get one millisecond to solve the captcha: too fast for a normal human, but it feels like hours to you.

The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.

For example:

1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
1111 produces 4 because each digit (all 1) matches the next.
1234 produces 0 because no digit matches the next.
91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
What is the solution to your captcha?

Your puzzle answer was 1044.

--- Part Two ---
You notice a progress bar that jumps to 50% completion. Apparently, the door isn't yet satisfied, but it did emit a star as encouragement. The instructions change:

Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.

For example:

1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
1221 produces 0, because every comparison is between a 1 and a 2.
123425 produces 4, because both 2s match each other, but no other digit has a match.
123123 produces 12.
12131415 produces 4.
What is the solution to your new captcha?

Your puzzle answer was 1054.
  */

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
