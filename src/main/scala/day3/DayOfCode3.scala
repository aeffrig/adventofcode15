package day3

import scala.io.Source

object DayOfCode3 {

  def directions(chars: List[Char]): (List[Int], List[Int]) = {
    chars.foldLeft((List[Int](), List[Int]())) {
      (t, c) => {
        val northOrSouth: List[Int] = t._1
        val eastOrWest: List[Int] = t._2
        c match {
          case '<' => (northOrSouth ::: 0 :: Nil, eastOrWest ::: -1 :: Nil)
          case '>' => (northOrSouth ::: 0 :: Nil, eastOrWest ::: 1 :: Nil)
          case '^' => (northOrSouth ::: 1 :: Nil, eastOrWest ::: 0 :: Nil)
          case 'v' => (northOrSouth ::: -1 :: Nil, eastOrWest ::: 0 :: Nil)
        }
      }
    }
  }

  def isCommandForSanta(t: (Char, Int)): Boolean = t._2 % 2 == 0

  def singleDirection(d:List[Int]) = d.scanLeft(0)(_+_)

  def main(args: Array[String]) {
    val fileAsString = Source.fromFile("day3input").mkString
    val d = directions(fileAsString.toList)
    val path = d._1.scanLeft(0)(_ + _) zip d._2.scanLeft(0)(_ + _)
    println(path.distinct.size)

    val withIndex = fileAsString.zipWithIndex

    val santa = withIndex.filter(isCommandForSanta).map(_._1).toList
    val roboSanta = withIndex.filterNot(isCommandForSanta).map(_._1).toList

    val sd = directions(santa)
    val sdns = singleDirection(sd._1)
    val sdew = singleDirection(sd._2)
    require(sdns.size == sdew.size)
    val santaPath = sdns zip sdew

    val rsd = directions(roboSanta)
    val rsdns = singleDirection(rsd._1)
    val rsdew = singleDirection(rsd._2)
    require(rsdns.size == rsdew.size)
    val roboSantaPath = rsdns zip rsdew

    println((santaPath ::: roboSantaPath ::: Nil).toSet.size)

  }
}
