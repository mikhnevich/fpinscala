package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/

 P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2

 */
object P03 {

  def nth[A](n: Int, x: List[A]): A = x(n)

  @tailrec
  def nth2[A](n: Int, x: List[A]): A = (n, x) match {
    case (0, a :: _) => a
    case (m, _ :: tail) => nth2(m - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }


}
