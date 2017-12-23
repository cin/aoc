package org.cinple.aoc

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

  private def mkSpiral: (Array[Array[Int]], CursorState) = {
    val mat = Array.ofDim[Int](dim, dim)
    val writer = new PrintWriter(new java.io.File("states"))
    val initState = CursorState(Point(hs, hs))
    var first = true
    var ans = true
    val res = (1 to input).foldLeft(initState) { case (cs, _) =>
      val v = if (first) { first = false; 1 } else sumAdjacent(cs.cursor, mat)
      if (v > input && ans) { println(s"~~~~: $v"); ans = false }
      writer.write(s"$cs, $v\n")
      mat(cs.cursor.c)(cs.cursor.r) = v
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

    val res = pts.foldLeft(0) { case (acc, pt) =>
      acc + spiral(pt.c)(pt.r)
    }
    res
  }

  override protected def part1(): Unit = {
    val spiral = mkSpiral
    println(s"final cursor state: (${spiral._2.cursor.c}, ${spiral._2.cursor.r}) ${spiral._2}")
    val writer = new PrintWriter(new java.io.File("mat"))
    spiral._1.foreach { rs =>
      rs.foreach { r => writer.write(f"$r%7d") }
      writer.write("\n")
    }
    writer.close()
  }

  override protected def part2(): Unit = {
  }
}
