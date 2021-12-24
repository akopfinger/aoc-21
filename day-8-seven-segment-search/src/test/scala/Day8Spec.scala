import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.Http

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Using.Manager.Resource
import scala.annotation.tailrec

class Day8Spec extends AnyFlatSpec with Matchers with AocUtils(day = "8") {

  trait SegmentCounter {
    def decodeRow(row: String): Int =
      val wireInput: Array[String] = row.split('|')(0).trim.split(' ').map(_.sorted)
      val numbers = row.split('|')(1).trim.split(' ').map(_.sorted)
      val decoder: Map[String, Int] =
        (wireInput.collect { in =>
          in.length match
            case 2 => 1 -> in
            case 3 => 7 -> in
            case 4 => 4 -> in
            case 7 => 8 -> in
        }.toMap).pipe { numToUnique =>
          (wireInput.collect { in =>
            in.length match
              case 5 =>
                in.diff(numToUnique(4)).length match
                  case 3 => in -> 2
                  case 2 =>
                    in.diff(numToUnique(7)).length match
                      case 3 => in -> 5
                      case _ => in -> 3
              case 6 =>
                in.diff(numToUnique(4)).length match
                  case 2 => in -> 9
                  case 3 =>
                    in.diff(numToUnique(7)).length match
                      case 4 => in -> 6
                      case _ => in -> 0
          }.toMap) ++ numToUnique.map((k, v) => v -> k)
        }
      numbers.map(decoder).mkString.toInt

    def countNumbersWithUniqueSegment(input: String): Int =
      def segmentIsUnique(segment: String): Boolean =
        segment.length.equals(2) || segment.length.equals(3) || segment.length.equals(4) || segment.length.equals(7)
      input.split("\n").map(_.split('|')).map(_(1).split(" ").count(segmentIsUnique)).sum
  }

  "part-1 example" should "count numbers with unique segments" in new SegmentCounter {
    val example = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".trim

    countNumbersWithUniqueSegment(example) shouldBe 26
  }

  "part-1" should "count numbers with unique segments" in new SegmentCounter {
    countNumbersWithUniqueSegment(aocInput) shouldBe 375
  }

  "part-2 example" should "take a row and decode the output" in new SegmentCounter {
    val example = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".trim.split("\n")

    example.map(row => decodeRow(row)).sum shouldBe 61229
  }

  "part-2" should "take a row and decode the output" in new SegmentCounter {
    val result = aocInput.split("\n").map(row => decodeRow(row))

    result.sum shouldBe 1019355
  }
}
