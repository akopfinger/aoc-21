import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.chaining.scalaUtilChainingOps
import cats.Monoid.combineAll
import scalaj.http.{Http, HttpOptions}

class Day2Spec extends AnyFlatSpec with Matchers {

  final val input =
    Http("https://adventofcode.com/2021/day/2/input").cookie("session", System.getenv("AOC_TOKEN")).asString.body

  sealed trait SubmarineHelmControlPart1 {
    val manoeuvreRegex = "(.*) (.*)".r

    def manoeuvreToPos(str: String): (Int, Int) = str match
      case manoeuvreRegex(direction, increment) =>
        direction match
          case "forward" => (increment.toInt, 0)
          case "down"    => (0, increment.toInt)
          case "up"      => (0, increment.toInt * -1)

    def calcDestination(manoeuvrs: Seq[String]) = combineAll(manoeuvrs.map(manoeuvreToPos)).pipe(t => t._1 * t._2)
  }

  sealed trait SubmarineHelmControlPart2 {
    val manoeuvreRegex = "(.*) (.*)".r

    def manoeuvreToPos(current: (Int, Int, Int), manoeuvre: String): (Int, Int, Int) = manoeuvre match
      case manoeuvreRegex(direction, increment) =>
        direction match
          case "forward" => (increment.toInt + current._1, current._2 + increment.toInt * current._3, current._3)
          case "down"    => (current._1, current._2, current._3 + increment.toInt)
          case "up"      => (current._1, current._2, current._3 - increment.toInt)

    def calcDestination(manoeuvrs: Seq[String]) = manoeuvrs.foldLeft((0, 0, 0))(manoeuvreToPos).pipe(t => t._1 * t._2)
  }

  "part-1 example" should "pilot this thing" in new SubmarineHelmControlPart1 {
    val exampleInput = Seq("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
    calcDestination(exampleInput) shouldBe 150
  }

  "part-1" should "multiply final horizontal position by final depth" in new SubmarineHelmControlPart1 {
    calcDestination(input.split("\n")) shouldBe 2117664
  }

  "part-2" should "account for aim and multiply final horizontal position by final depth" in new SubmarineHelmControlPart2 {
    calcDestination(input.split("\n")) shouldBe 2073416724
  }
}
