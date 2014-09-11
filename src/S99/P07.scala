package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/

P07 (**) Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)

 */
object P07 {

  //  def flatten2(x: List): List = x.foldLeft(List())((acc, element) => acc ++ flatten(element))

  def flatten(x: Any): List[Any] = x match {
    case Nil => Nil
    case List(h) :: Nil =>
      val a = flatten(h)
      a
    case List(h) :: tail =>
      val a = flatten(h)
      val b = flatten(tail)
      val c = a ++ b
      c
    case h :: tail =>
      val a = flatten(tail)
      val b = h :: a
      b
  }


  def main(args: Array[String]) {
    //    println(flatten(List(List(1), 2)))
    println(flatten(List(List(1, 2))))
    //        println(flatten(List(List(1, 1), 2)))
    //        println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }

}
