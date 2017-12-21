package org.cinple.aoc

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
