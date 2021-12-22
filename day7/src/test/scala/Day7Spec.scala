import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Using.Manager.Resource
import scala.annotation.tailrec

class Day7Spec extends AnyFlatSpec with Matchers {
  extension [A, B](a: A) def |(f: (A) => B): B = a.pipe(f)
  extension (s: String) def splitAsInt = s.split(',').map(_.toInt)

  final lazy val aocInput: String =
    Http("https://adventofcode.com/2021/day/7/input").cookie("session", System.getenv("AOC_TOKEN")).asString.body.trim

  trait CrabCalc {
    def getLinearFuelMin(input: Array[Int]): Int =
      Range(input.min, input.max).map(c => input.map(i => (c - i).abs).sum).min

    def getExponentialFuelMin(input: Array[Int]): Int =
      Range(input.min, input.max)
        .map(i => input.map(pos => (i - pos).abs).map(steps => steps * (steps + 1) / 2).sum)
        .min
  }

  "part-1 example" should "calculate the least amount of fuel needed" in new CrabCalc {
    val input = "16,1,2,0,4,2,7,1,2,14" | splitAsInt

    getLinearFuelMin(input) shouldBe 37
  }

  "part-1" should "calculate the least amount of fuel needed" in new CrabCalc {
    val input = aocInput | splitAsInt

    getLinearFuelMin(input) shouldBe 357353
  }

  "part-2" should "calculate exponential fuel cost and get min" in new CrabCalc {
    val input = aocInput | splitAsInt

    getExponentialFuelMin(input) shouldBe 104822130
  }
}
