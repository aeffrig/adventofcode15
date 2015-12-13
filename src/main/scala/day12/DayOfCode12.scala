package day12

import io.circe._
import io.circe.jawn.JawnParser

import scala.io.Source

object DayOfCode12 {

  def replaceWithSpace(s: String, c: Char): String = s.replace(c, ' ')

  def splitOnSpace(s: String) = s.trim.split("\\s+")

  def jsonNumber(n: JsonNumber): Int = n.toInt.getOrElse(0)

  def jsonNull = 0

  def jsonBoolean(x: Boolean) = 0

  def jsonString(x: String) = 0

  def jsonObject(x: JsonObject): Int = if (x.values.exists(f => f.isString && f.asString.exists(s => s.equals("red")))) {
    0
  }
  else {
    x.values.foldLeft(0) { (total: Int, a: Json) => total + a.fold(jsonNull, jsonBoolean, jsonNumber, jsonString, jsonArray, jsonObject) }
  }


  def jsonArray(x: List[Json]): Int = x.foldLeft(0) { (total: Int, a: Json) => total + a.fold(jsonNull, jsonBoolean, jsonNumber, jsonString, jsonArray, jsonObject) }

  def main(args: Array[String]) {

    val charsToReplace = Set('{', '}', ',', '"', '[', ']', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
      'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', ':')
    val file = Source.fromFile("day12input").mkString

    val listOfNums = splitOnSpace(charsToReplace.foldLeft(file)(replaceWithSpace)).toList

    val asInt = listOfNums map { s => s.toInt }

    val total = asInt.reduce(_ + _)

    println(total)

    val total2 = new JawnParser().parse(Source.fromFile("day12input").mkString) map {
      json => json.fold(jsonNull, jsonBoolean, jsonNumber, jsonString, jsonArray, jsonObject)
    }

    println(total2)
  }
}
