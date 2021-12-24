import scalaj.http.Http
import java.nio.file.Paths
import java.nio.file.Files
import scala.util.chaining.scalaUtilChainingOps

trait AocUtils(year: String = "2021", day: String) {
  extension [A, B](a: A) def |(f: (A) => B): B = a.pipe(f)
  extension (s: String) def splitAsInt = s.split(',').map(_.toInt)

  final lazy val aocInput =
    val inputPath = Paths.get(s"${System.getProperty("java.io.tmpdir")}/aoc$year/day$day.txt")
    lazy val fetchedInput = Http(s"https://adventofcode.com/$year/day/$day/input")
      .cookie("session", System.getenv("AOC_TOKEN"))
      .asString
      .body
      .trim
    if Files.exists(inputPath) then Files.readString(inputPath)
    else
      Files.createDirectories(inputPath.getParent)
      Files.createFile(inputPath)
      fetchedInput.tap(input => Files.write(inputPath, input.getBytes))
}
