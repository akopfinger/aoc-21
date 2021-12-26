import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Using.Manager.Resource
import scala.annotation.tailrec

class Day9Spec extends AnyFlatSpec with Matchers with AocUtils(day = "9") {

  trait HeighMapFunctions {
    def getPoints(
        input: Array[Array[Char]],
        compare: => (c: Char, other: Option[Char]) => Boolean
    ): Array[(Int, Int)] = {
      def rows = input.lift
      input.zipWithIndex.flatMap { case (r, y) =>
        def row = r.lift
        r.zipWithIndex.collect {
          case (c, x)
              if getAdjacent(input, x, y).pipe((left, right, up, down) =>
                compare(c, left) && compare(c, right) && compare(c, up) && compare(c, down)
              ) =>
            (y, x)
        }
      }
    }

    /** @return A tuple of (left, right, up down) */
    def getAdjacent(
        input: Array[Array[Char]],
        x: Int,
        y: Int
    ): (Option[Char], Option[Char], Option[Char], Option[Char]) =
      def rows = input.lift
      rows(y)
        .map(_.lift.pipe { row =>
          (
            row(x - 1),
            row(x + 1),
            rows(y - 1).flatMap(_.lift(x)),
            rows(y + 1).flatMap(_.lift(x))
          )
        })
        .getOrElse((None, None, None, None))

    def getLowPoints(input: Array[Array[Char]]): Array[(Int, Int)] =
      getPoints(input, (c: Char, other: Option[Char]) => other.forall(_ > c))

    @tailrec
    final def expandBasin(
        pois: Array[(Int, Int)],
        area: Array[Array[Char]],
        basin: Array[(Int, Int)]
    ): Array[(Int, Int)] = {
      if pois.nonEmpty then
        val (y, x) = pois(0)
        val (left, right, up, down) = getAdjacent(area, x, y)
        expandBasin(
          pois = (pois.drop(1) ++ Array(
            left.collect { case c if c < '9' => (y, x - 1) },
            right.collect { case c if c < '9' => (y, x + 1) },
            up.collect { case c if c < '9' => (y - 1, x) },
            down.collect { case c if c < '9' => (y + 1, x) }
          ).flatten).diff(basin),
          area = area,
          basin = (basin ++ Array(pois(0)))
        )
      else basin.distinct
    }
  }

  "part-1 example" should "count numbers with unique segments" in new HeighMapFunctions {
    val input = """2199943210,3987894921,9856789892,8767896789,9899965678""".trim
      .split(',')
      .map(_.toCharArray)

    getLowPoints(input)
      .map((i, n) => input(i)(n).getNumericValue + 1)
      .sum shouldBe 15
  }

  "part-1" should "count numbers with unique segments" in new HeighMapFunctions {
    val input: Array[Array[Char]] = aocInput | splitNewLines | asCharArrays

    getLowPoints(input)
      .map((i, n) => input(i)(n).getNumericValue + 1)
      .sum shouldBe 524
  }

  "part 2 example" should "take the 3 largest basins and multiply their sizes " in new HeighMapFunctions {
    val input = """2199943210,3987894921,9856789892,8767896789,9899965678""".trim
      .split(',')
      .map(_.toCharArray)

    getLowPoints(input)
      .map(poi => expandBasin(Array(poi), input, Array(poi)).length)
      .sorted
      .reverse
      .take(3)
      .foldLeft(1)(_ * _) shouldBe 1134
  }

  "part 2" should "take the 3 largest basins and multiply their sizes " in new HeighMapFunctions {
    val input: Array[Array[Char]] = aocInput | splitNewLines | asCharArrays

    getLowPoints(input)
      .map(poi => expandBasin(Array(poi), input, Array(poi)).length)
      .sorted
      .reverse
      .take(3)
      .foldLeft(1)(_ * _) shouldBe 1235430
  }
}
