import scala.util.matching.Regex
import scala.io.Source
val testLines = Source.fromResource("2-test.txt").getLines
val runLines = Source.fromResource("2.txt").getLines()
case class Hand(red: Int, blue: Int, green: Int)
case class Game(number: Int, hands: Array[Hand])
def parseLine(line: String): Game = {
  def getColorCount(hand: String, regex: Regex): Int =
    regex.findFirstMatchIn(hand).map(_.group(1).toInt).orElse(Some(0)).get
  val redExp = "(\\d+)\\s+red".r
  val blueExp = "(\\d+)\\s+blue".r
  val greenExp = "(\\d+)\\s+green".r
  val gamesplit = line.split(":")
  val gameNum = gamesplit(0).filter(_.isDigit).toInt
  Game(
    gameNum,
    gamesplit(1)
      .split(";")
      .map(handStr => {
        Hand(
          getColorCount(handStr, redExp),
          getColorCount(handStr, blueExp),
          getColorCount(handStr, greenExp)
        )
      })
  )
}
def partOne(lines: Iterator[String]): Int = {
  lines
    .map(parseLine(_))
    .map(game =>
      (
        game.number,
        game.hands.count(x => x.red > 12 || x.green > 13 || x.blue > 14)
      )
    )
    .filter(_._2 == 0)
    .map(_._1)
    .sum
}
def partTwo(lines: Iterator[String]): Int = {
  lines
    .map(parseLine(_))
    .map(_.hands)
    .map(hands => {
      Hand(
        hands.maxBy(_.red).red,
        hands.maxBy(_.blue).blue,
        hands.maxBy(_.green).green
      )
    })
    .map(max => max.green * max.red * max.blue)
    .sum
}
val testAnswerPartOne = partOne(testLines)
val answerPartOne = partOne(runLines)
val testAnswerPartTwo = partTwo(testLines)
val answerPartTwo = partTwo(runLines)
