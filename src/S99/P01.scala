package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/
 * P01 (*) Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
 */
object P01 {
  def last[A](x: List[A]): A = x.last // :)

  @tailrec
  def last2[A](x: List[A]): A = x match {
    case a :: Nil => a
    case _ :: tail => last2(tail)
    case _ => throw new NoSuchElementException
  }

}
