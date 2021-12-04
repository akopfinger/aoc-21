import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

class Day4Spec extends AnyFlatSpec with Matchers {
  extension [A, B](a: A) def |(f: (A) => B): B = a.pipe(f)
  extension (xs: Array[String]) def asCharArrays = xs.map(_.toArray)
  extension (xs: Array[String]) def asIntArrays = xs.map(_.toArray.map(_.toString.toInt))
  extension (xs: Array[String]) def trimValues = xs.map(_.trim)
  extension (xs: Array[String]) def toVector = xs.toVector
  extension (s: String) def splitLines = s.split('\n')

  final lazy val aocInput: Array[String] =
    Http("https://adventofcode.com/2021/day/4/input")
      .cookie("session", System.getenv("AOC_TOKEN"))
      .asString
      .body
      .split("\n")

  trait SubmarineRecreationalFunctions {
    def getMoves(lines: Vector[String]): List[String] = lines
      .filter(_.contains(','))
      .mkString
      .split(',')
      .toList

    def getBoards(lines: Vector[String]): Vector[Vector[Vector[String]]] = lines
      .filterNot(l => l.contains(',') || l.isEmpty)
      .sliding(5, 5)
      .toVector
      .map(_.map(_.split(' ').filter(_.nonEmpty).toVector))

    def rowIsWinner(row: Vector[String]) = row.forall(_.contains('x'))

    def scoreBoard(multiplier: String, board: Vector[Vector[String]]): Int =
      board
        .flatMap(_.filterNot(_.contains("x")))
        .map(_.toInt)
        .sum * multiplier.toInt

    @tailrec
    final def getWinningDrawGroup(
        moves: List[String],
        boards: Vector[Vector[Vector[String]]],
        firstWinner: Option[(String, Vector[Vector[String]])] = None,
        lastWinner: Option[(String, Vector[Vector[String]])] = None
    ): (Option[(String, Vector[Vector[String]])], Option[(String, Vector[Vector[String]])]) =
      moves match
        case Nil => (firstWinner, lastWinner)
        case _ =>
          val draw = moves.head
          val marked: Vector[Vector[Vector[String]]] = boards
            .map(_.map(row => row.map(slot => if slot.equals(draw) then s"x$slot" else slot)))
          val xwinners = marked.filter(_.exists(rowIsWinner))
          val ywinners = marked.filter(_.transpose.exists(rowIsWinner))
          getWinningDrawGroup(
            moves = moves.drop(1),
            boards = marked.diff(xwinners).diff(ywinners),
            firstWinner = firstWinner
              .orElse(xwinners.++(ywinners).headOption.map(w => (draw, w))),
            lastWinner = xwinners
              .++(ywinners)
              .lastOption
              .map(w => (draw, w))
              .orElse(lastWinner)
          )
  }

  "part-1 example" should "return power consumption" in new SubmarineRecreationalFunctions {
    val maybeWinner = getWinningDrawGroup(getMoves(inputExample1), getBoards(inputExample1))
    maybeWinner._1.isDefined shouldBe (true)
    maybeWinner._1.map { (draw, board) =>
      val sum = board
        .flatMap(_.filterNot(_.contains("x")))
        .map(_.toInt)
        .sum
      sum shouldBe 188
      draw.toInt shouldBe 24
      draw.toInt * sum shouldBe 4512
    }
  }

  "part-1" should "return final score of winner" in new SubmarineRecreationalFunctions {
    val input = aocInput | trimValues | toVector
    val score = getWinningDrawGroup(getMoves(input), getBoards(input))._1
      .map(scoreBoard)
      .get shouldBe 39902
  }

  "part-2 example" should "return final score of last winner" in new SubmarineRecreationalFunctions {
    val last = getWinningDrawGroup(getMoves(inputExample1), getBoards(inputExample1))._2
    last.map(scoreBoard).get shouldBe 1924
  }

  "part-2" should "return final score of last winner" in new SubmarineRecreationalFunctions {
    val input = aocInput | trimValues | toVector
    val last = getWinningDrawGroup(getMoves(input), getBoards(input))._2
    last.map(scoreBoard).get shouldBe 26936
  }

  val inputExample1: Vector[String] =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7""" | splitLines | trimValues | toVector
}
