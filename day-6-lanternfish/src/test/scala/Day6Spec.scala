import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Using.Manager.Resource
import scala.annotation.tailrec

class Day6Spec extends AnyFlatSpec with Matchers {
  extension [A, B](a: A) def |(f: (A) => B): B = a.pipe(f)
  extension (s: String) def splitAsInt = s.split(',').map(_.toInt)

  final lazy val aocInput: String =
    Http("https://adventofcode.com/2021/day/6/input")
      .cookie("session", System.getenv("AOC_TOKEN"))
      .asString
      .body
      .trim

  trait FishScience {
    @tailrec
    final def countFishs(days: Int, fishcounter: Array[Long]): Array[Long] =
      if days == 0 then fishcounter
      else
        countFishs(
          (days - 1),
          fishcounter(0).pipe(births =>
            fishcounter.drop(1).appended(births).tap(_(6) += births)
          )
        )
  }

  "part-1" should "calculate expected fishes after 80 days" in new FishScience {
    val fishcounter = Array.fill(9)(0L)
    (aocInput | splitAsInt).foreach(i => fishcounter(i) += 1)

    countFishs(80, fishcounter).sum shouldBe 379114L
  }

  "part-2" should "calculate expected fishes after 256 days" in new FishScience {
    val fishcounter = Array.fill(9)(0L)
    (aocInput | splitAsInt).foreach(i => fishcounter(i) += 1)

    countFishs(256, fishcounter).sum shouldBe 1702631502303L
  }
}
