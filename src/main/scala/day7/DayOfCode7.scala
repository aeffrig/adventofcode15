package day7

import scala.collection.mutable.Map
import collection.mutable.{HashMap, MultiMap, Set}
import scala.io.Source
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed trait Gate

final case class Not(output: String) extends Gate with (Int => Int) {
  override def apply(x: Int) = ~x
}

final case class Or2(output: String, input: String) extends Gate with (Int => Or1) {
  override def apply(x: Int) = Or1(output, x)
}

final case class Or1(output: String, x: Int) extends Gate with (Int => Int) {
  override def apply(y: Int) = x | y
}

final case class And2(output: String, input: String) extends Gate with (Int => And1) {
  override def apply(x: Int) = And1(output, x)
}

final case class And1(output: String, x: Int) extends Gate with (Int => Int) {
  override def apply(y: Int) = x & y
}

final case class LShift(output: String, s: Int) extends Gate with (Int => Int) {
  override def apply(x: Int) = x << s
}

final case class RShift(output: String, s: Int) extends Gate with (Int => Int) {
  override def apply(x: Int) = x >> s
}

final case class Wire(output: String) extends Gate with (Int => Int) {
  override def apply(x: Int) = x
}

object DayOfCode7 {

  def main(args: Array[String]): Unit = {
    val mm = new HashMap[String, Set[Gate]] with MultiMap[String, Gate]

    var vals = Map[String, Int]()

    def interpret(b: String, v: Int): Unit = {
      val g = mm.get(b)
      g map {
        z =>
          z.foreach { r =>
            r match {
              case f@Not(o) => vals += o -> f(v); mm.removeBinding(b, f);
              case f@Or2(o, i) => mm.removeBinding(b, f); mm.addBinding(i, f(v));
              case f@Or1(o, x) => mm.removeBinding(b, f); vals += o -> f(v);
              case f@And2(o, i) => mm.removeBinding(b, f); mm.addBinding(i, f(v));
              case f@And1(o, x) => mm.removeBinding(b, f); vals += o -> f(v);
              case f@LShift(o, x) => mm.removeBinding(b, f); vals += o -> f(v);
              case f@RShift(o, x) => mm.removeBinding(b, f); vals += o -> f(v);
              case f@Wire(o) => mm.removeBinding(b, f); vals += o -> f(v);
            }
          }
      }
    }

    Source.fromFile("day7input").getLines().toList foreach { line =>
      line.trim.split(" ").toList match {
        case "NOT" :: v :: "->" :: o :: Nil => mm.addBinding(v, Not(o))
        case a :: "OR" :: b :: "->" :: o :: Nil => mm.addBinding(a, Or2(o, b)); mm.addBinding(b, Or2(o, a))
        case a :: "AND" :: b :: "->" :: o :: Nil => { Try[Int](a.toInt) match {
          case Failure(_) => {
            mm.addBinding(a, And2(o, b))
            mm.addBinding(b, And2(o, a))
          }
          case Success(n) => {
            mm.addBinding(b, And1(o, n))

          }
        }

        }
        case a :: "LSHIFT" :: b :: "->" :: o :: Nil => mm.addBinding(a, LShift(o, b.toInt))
        case a :: "RSHIFT" :: b :: "->" :: o :: Nil => mm.addBinding(a, RShift(o, b.toInt))
        case v :: "->" :: o :: Nil => Try(v.toInt) match {
          case Failure(_) => mm.addBinding(v, Wire(o))
          case Success(x) => vals += (o -> v.toInt)
        }
        case unknown@_ => println(unknown)
      }
    }

    println(mm)

    vals += ("b" -> 3176)

    (1 to 1000).toList foreach { _ =>
      vals.iterator.toList.foreach {
        t => interpret(t._1, t._2)
      }
    }

    println(mm)
    println(vals)
    println(vals.keys.toList.sorted)
    println(mm.keys.toList.sorted)

  }


}
