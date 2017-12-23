package org.cinple.aoc

trait aoc {
  protected def part1(): Unit
  protected def part2(): Unit

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
