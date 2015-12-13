package day13

import scala.io.Source
import scala.collection.mutable.Map

object DayOfCode13 {
  def main(args: Array[String]) {
    val happiness = Map[(String, String), Int]()


    val people = Source.fromFile("day13input").getLines().filterNot(_.isEmpty).toList.flatMap { line =>
      line.trim.replace(".", "").split(" ").toList match {
        // Alice would gain 2 happiness units by sitting next to Bob.
        case (x: String) :: "would" :: "gain" :: (a: String) :: "happiness" :: "units" :: "by" :: "sitting" :: "next" :: "to" :: (y: String) :: Nil => happiness.put((x, y), a.toInt);  Some(x)
        case (x: String) :: "would" :: "lose" :: (a: String) :: "happiness" :: "units" :: "by" :: "sitting" :: "next" :: "to" :: (y: String) :: Nil => happiness.put((x, y), -a.toInt); Some(x)
        case _ => None
      }
    }.toSet.toList

    val perms = permutations(people, happiness)
    println(perms.max)

    val peopleAndMe = people ::: "Me" :: Nil
    people foreach { person =>
      happiness.put((person, "Me"), 0);
      happiness.put(("Me", person), 0);
    }

    val permsWithMe = permutations(peopleAndMe, happiness)
    println(permsWithMe.max)

  }

  def permutations(people: List[String], happiness:Map[(String, String), Int]): List[Int] = {
    val size = people.size
    val perms = people.toSet[String].toList.permutations.toList.map { list =>
      list.sliding(2).toList.map { s => s match {
        case s1 :: s2 :: Nil => happiness.get((s1, s2)).get + happiness.get((s2, s1)).get
      }
      }.reduce(_ + _) + happiness.get(list(0), list(size - 1)).get + happiness.get(list(size - 1), list(0)).get

    }
    perms
  }
}
