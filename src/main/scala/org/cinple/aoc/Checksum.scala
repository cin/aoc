package org.cinple.aoc

/**
  * --- Day 2: Corruption Checksum ---
As you walk through the door, a glowing humanoid shape yells in your direction. "You there! Your state appears to be idle. Come help us repair the corruption in this spreadsheet - if we take another millisecond, we'll have to display an hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

For example, given the following spreadsheet:

5 1 9 5
7 5 3
2 4 6 8
The first row's largest and smallest values are 9 and 1, and their difference is 8.
The second row's largest and smallest values are 7 and 3, and their difference is 4.
The third row's difference is 6.
In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

What is the checksum for the spreadsheet in your puzzle input?

Your puzzle answer was 44216.

--- Part Two ---
"Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program seems a little worried. Can programs be worried?

"Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."

It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.

For example, given the following spreadsheet:

5 9 2 8
9 4 7 3
3 8 6 5
In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
In the second row, the two numbers are 9 and 3; the result is 3.
In the third row, the result is 2.
In this example, the sum of the results would be 4 + 3 + 2 = 9.

What is the sum of each row's result in your puzzle input?

Your puzzle answer was 320.
  */

import scala.io.Source.fromInputStream

object Checksum {
  private val inputFn = "/checksum-input.txt"

  private def part1(): Unit = {
    val r1 = Seq(5, 1, 9, 5)
    val r2 = Seq(7, 5, 3)
    val r3 = Seq(2, 4, 6, 8)
    val res = Seq(r1, r2, r3).foldLeft(0L) { case (acc, rs) =>
      acc + (rs.max - rs.min)
    }
    assert(res == 18)

    val stream = getClass.getResourceAsStream(inputFn)
    val checksum = fromInputStream(stream)
      .getLines
      .map { _.split("\t").map(_.toInt) }
      .foldLeft(0) { case (acc, rs) =>
        acc + (rs.max - rs.min)
      }
    println(checksum)
  }

  private def combo(mx: Seq[Seq[Int]]): Int = mx.foldLeft(0) { case (acc, rs) =>
    rs.combinations(2).foldLeft(acc) {
      case (acc1, Seq(a, b)) if a % b == 0 => acc1 + (a / b)
      case (acc1, Seq(a, b)) if b % a == 0 => acc1 + (b / a)
      case (acc1, _) => acc1
    }
  }

  private def part2(): Unit = {
    val r1 = Seq(5, 9, 2, 8)
    val r2 = Seq(9, 4, 7, 3)
    val r3 = Seq(3, 8, 6, 5)
    assert(combo(Seq(r1, r2, r3)) == 9)

    val stream = getClass.getResourceAsStream(inputFn)
    val data = fromInputStream(stream)
      .getLines
      .map { _.split("\t").map(_.toInt).toSeq }.toSeq
    println(combo(data))
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
