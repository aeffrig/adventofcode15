package day1

import scala.io.Source

object DayofCode1 {
  def main(args: Array[String]) {
    val answerPart1 = Source.fromFile("day1input").mkString.map { case '(' => 1
    case ')' => -1 }.reduce(_ + _)
    println(answerPart1)

    val answerPart2 = Source.fromFile("day1input").mkString.map { case '(' => 1
    case ')' => -1 }.scanLeft(0)(_ + _).indexOf(-1)
    println(answerPart2)
  }
}
