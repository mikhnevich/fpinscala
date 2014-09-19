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

  def flatten(x: List[Any]): List[Any] = x flatMap {
    case ls: List[_] => flatten(ls)
    case e => List(e)
  }


  def main(args: Array[String]) {
            println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }

}
