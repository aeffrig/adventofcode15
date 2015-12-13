package day2

import scala.io.Source

object DayOfCode2 {

  def smallestSide(l: Int, w:Int, h:Int):Int = List(l, w, h).combinations(2).map(side => side.reduce(_ * _)).min

  def surfaceArea(l: Int, w: Int, h: Int):Int = 2 * l * w + 2 * w * h + 2 * h * l

  def parse(s: String): List[Int] = s.split("x").toList.map { x => x.toInt }

  def bow(l:Int, w:Int, h:Int) = l * w * h

  def ribbon(l:Int, w:Int, h:Int) = List(l, w, h).combinations(2).map(side =>
    side.foldLeft(0)((total:Int, l:Int) => total + l + l)).min

  def main(args: Array[String]) {
    val part1answer:List[Int] = Source.fromFile("day2input").getLines().toList map { line =>
      parse(line) match {
        case l :: w :: h :: Nil => surfaceArea(l, w, h) + smallestSide(l,w,h)
        case _ => 0
      }
    }
    println(part1answer.reduce(_+_))

    val part2answer:List[Int] = Source.fromFile("day2input").getLines().toList map { line =>
      parse(line) match {
        case l :: w :: h :: Nil => ribbon(l, w, h) + bow(l,w,h)
        case _ => 0
      }
    }
    println(part2answer.reduce(_+_))
  }

}
