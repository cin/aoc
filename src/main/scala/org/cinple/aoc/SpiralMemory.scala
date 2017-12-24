package org.cinple.aoc

/**
  * --- Day 3: Spiral Memory ---
You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.
How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

Your puzzle answer was 552.

--- Part Two ---
As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
What is the first value written that is larger than your puzzle input?

Your puzzle answer was 330785.

Both parts of this puzzle are complete! They provide two gold stars: **

At this point, you should return to your advent calendar and try another puzzle.

Your puzzle input was 325489.
  */

import java.io.PrintWriter

case class Point(c: Int, r: Int)

trait Dir
case object Right extends Dir
case object Up extends Dir
case object Left extends Dir
case object Down extends Dir

case class CursorState(cursor: Point, dir: Dir, mn: Point, mx: Point)

object CursorState {
  def apply(cursor: Point): CursorState = CursorState(
    cursor,
    Right,
    cursor,
    cursor
  )
}

// pretty sure there is something wrong with this but the solution to this problem is symmetrical and
// accommodated for some sloppiness here. could investigate but need to move on...
object SpiralMemory extends aoc {
  private val input = 325489
//  private val input = 68
  private val dim = math.ceil(math.sqrt(input)).toInt + 1
  private val hs = math.ceil(dim / 2).toInt - 1

  private def mvCursor(c: CursorState): CursorState = c.dir match {
    case Right if c.cursor.c + 1 > c.mx.c => CursorState(Point(c.cursor.c, c.cursor.r + 1), Up, c.mn, Point(c.mx.c, c.mx.r + 1))
    case Right => CursorState(Point(c.cursor.c + 1, c.cursor.r), Right, c.mn, c.mx)
    case Up if c.cursor.r + 1 > c.mx.r => CursorState(Point(c.cursor.c - 1, c.cursor.r), Left, Point(c.mn.c - 1, c.mn.r), c.mx)
    case Up => CursorState(Point(c.cursor.c, c.cursor.r + 1), Up, c.mn, c.mx)
    case Left if c.cursor.c - 1 < c.mn.c => CursorState(Point(c.cursor.c, c.cursor.r - 1), Down, Point(c.mn.c, c.mn.r - 1), c.mx)
    case Left => CursorState(Point(c.cursor.c - 1, c.cursor.r), Left, c.mn, c.mx)
    case Down if c.cursor.r - 1 < c.mn.r => CursorState(Point(c.cursor.c + 1, c.cursor.r), Right, c.mn, Point(c.mx.c + 1, c.mx.r))
    case Down => CursorState(Point(c.cursor.c, c.cursor.r - 1), Down, c.mn, c.mx)
    case e => throw new Exception(s"Unhandled case $e")
  }

  private def mkSpiral(part2: Boolean = false): (Array[Array[Int]], CursorState) = {
    val mat = Array.ofDim[Int](dim, dim)
    val writer = new PrintWriter(new java.io.File(s"spiral-states${if (part2) "2"}.txt"))
    val initState = CursorState(Point(hs, hs))
    var first = true
    var ans = true
    val res = (1 to input).foldLeft(initState) { case (cs, i) =>
      val v = if (first) { first = false; 1 } else sumAdjacent(cs.cursor, mat)
      if (v > input && ans) { println(s"~~~~: $v"); ans = false }
      if (part2) {
        writer.write(s"$cs, $v\n")
        mat(cs.cursor.c)(cs.cursor.r) = v
      } else {
        writer.write(s"$cs, $i\n")
        mat(cs.cursor.c)(cs.cursor.r) = i
      }
      mvCursor(cs)
    }
    writer.close()
    (mat, res)
  }

  private def sumAdjacent(c: Point, spiral: Array[Array[Int]]): Int = {
    val pts = new scala.collection.mutable.HashSet[Point]()
    if (c.c - 1 >= 0) {
      if (c.r - 1 >= 0) pts += Point(c.c - 1, c.r - 1)
      pts += Point(c.c - 1, c.r)
      if (c.r + 1 <= dim) pts += Point(c.c - 1, c.r + 1)
    }

    if (c.c + 1 <= dim) {
      if (c.r + 1 <= dim) pts += Point(c.c + 1, c.r + 1)
      pts += Point(c.c + 1, c.r)
      if (c.r - 1 >= 0) pts += Point(c.c + 1, c.r - 1)
    }

    if (c.r + 1 <= dim) pts += Point(c.c, c.r + 1)
    if (c.r - 1 >= 0) pts += Point(c.c, c.r - 1)

    pts.foldLeft(0) { case (acc, pt) => acc + spiral(pt.c)(pt.r) }
  }

  override protected def part1(): Unit = {
    val spiral = mkSpiral()
    println(s"final cursor state: (${spiral._2.cursor.c}, ${spiral._2.cursor.r}) ${spiral._2}")
    val writer = new PrintWriter(new java.io.File("spiral-mat.txt"))
    spiral._1.foreach { rs =>
      rs.foreach { r => writer.write(f"$r%7d") }
      writer.write("\n")
    }
    writer.close()
  }

  override protected def part2(): Unit = {
    val spiral = mkSpiral(true)
    val writer = new PrintWriter(new java.io.File("sprial-mat2.txt"))
    spiral._1.foreach { rs =>
      rs.foreach { r => writer.write(f"$r%7d") }
      writer.write("\n")
    }
    writer.close()
  }
}
