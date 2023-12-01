import scala.io.Source

def firstPart(line: String): Int = {
  val digits = line.filter(_.isDigit)
  val head = digits.headOption match {
    case Some(x) => x
    case None    => 0
  }

  val last = digits.lastOption match {
    case Some(x) => x
    case None    => 0
  }
  s"$head$last".toInt
}

val digitMap = Map(
  "zero" -> 0,
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9,
  "1" -> 1,
  "2" -> 2,
  "3" -> 3,
  "4" -> 4,
  "5" -> 5,
  "6" -> 6,
  "7" -> 7,
  "8" -> 8,
  "9" -> 9
)
def secondPart(line: String): Int = {
  val regex = "(?=([1-9]|one|two|three|four|five|six|seven|eight|nine))".r
  val digits = regex.findAllMatchIn(line).map(_.group(1)).toVector
  val head = digits.headOption match {
    case Some(x) =>
      digitMap(x).toInt
    case None => 0
  }

  val last = digits.lastOption match {
    case Some(x) =>
      digitMap(x).toInt
    case None => 0
  }
  s"$head$last".toInt
}
val firstTestValue =
  Source.fromResource("1-test1.txt").getLines.map(firstPart(_)).sum
val firstValue = Source.fromResource("1.txt").getLines.map(firstPart(_)).sum
val secondTestValue =
  Source.fromResource("1-test.txt").getLines.map(secondPart(_)).sum
val secondValue = Source.fromResource("1.txt").getLines().map(secondPart(_)).sum
