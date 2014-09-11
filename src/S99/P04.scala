package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/

P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6

 */
object P04 {

  def length[A](x: List[A]): Int = x.length

  def length2[A](x: List[A]): Int = x.foldLeft(0)((acc, _) => acc + 1)


}
