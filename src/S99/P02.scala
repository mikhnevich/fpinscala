package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/
P02 (*) Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
 */
object P02 {
  def penultimate[A](x: List[A]): A = x.init.last

  @tailrec
  def penultimate2[A](x: List[A]): A = x match {
    case a :: _ :: Nil => a
    case _ :: tail => penultimate2(tail)
    case _ => throw new NoSuchElementException
  }


}
