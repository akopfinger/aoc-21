import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps

class Day5Spec extends AnyFlatSpec with Matchers {
  extension [A, B](a: A) def |(f: (A) => B): B = a.pipe(f)
  extension (xs: Array[String]) def asCharArrays = xs.map(_.toArray)
  extension (xs: Array[String]) def asIntArrays = xs.map(_.toArray.map(_.toString.toInt))
  extension (xs: Array[String]) def trimValues = xs.map(_.trim)
  extension (xs: Array[String]) def toVector = xs.toVector
  extension (s: String) def splitLines = s.split('\n')

  final lazy val aocInput: Array[String] =
    Http("https://adventofcode.com/2021/day/5/input")
      .cookie("session", System.getenv("AOC_TOKEN"))
      .asString
      .body
      .split("\n")

  trait SubmarineSeismologyFunctions {
    val reg = "(.*),(.*) -> (.*),(.*)".r

    case class Line(x1: Int, x2: Int, y1: Int, y2: Int) {
      def stepDir(start: Int, end: Int): Int = if (start > end) then -1 else 1

      def getCoordinates: Vector[(Int, Int)] =
        if isDiagonal then getDiagonalCoordinates
        else
          (if isHorizontal then for (i <- y1 to y2 by stepDir(y1, y2)) yield (x1, i)
           else for (i <- x1 to x2 by stepDir(x1, x2)) yield (i, y1)).toVector

      def getDiagonalCoordinates: Vector[(Int, Int)] = {
        val xs = for (x <- x1 to x2 by stepDir(x1, x2)) yield x
        val ys = for (y <- y1 to y2 by stepDir(y1, y2)) yield y

        xs.zipWithIndex.map(((x, i) => (x, ys(i)))).toVector
      }

      def isHorizontal: Boolean = x1 == x2
      def isVertical: Boolean = y1 == y2
      def isDiagonal: Boolean = !isHorizontal && !isVertical
    }

    def countOverlapping(lines: Vector[Line], includeDiagonal: Boolean = false): Int = {
      lines
        .filter(r => includeDiagonal || (r.isHorizontal || r.isVertical))
        .flatMap(_.getCoordinates)
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .count { case (_, n) => n > 1 }
    }
  }

  "part-1" should "count overlapping vents" in new SubmarineSeismologyFunctions {
    val input = aocInput | trimValues | toVector
    val lines = input.collect { case reg(x1, y1, x2, y2) =>
      Line(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    }
    countOverlapping(lines) shouldBe 7142
  }

  "part-2" should "include countDiagonals" in new SubmarineSeismologyFunctions {
    val input = aocInput | trimValues | toVector
    val lines = input.collect { case reg(x1, y1, x2, y2) =>
      Line(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    }

    countOverlapping(lines, includeDiagonal = true) shouldBe 2
  }
}
