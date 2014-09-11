package S99

/**
 * http://aperiodic.net/phil/scala/s-99/

P05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1,

 */
object P05 {

  def reverse[A](x: List[A]): List[A] = x.reverse

  def reverse2[A](x: List[A]): List[A] = x match {
    case Nil => Nil
    case (a :: tail) => reverse2(tail) ::: List(a)
  }

  def reverse3[A](x: List[A]): List[A] = x.foldRight(List[A]())((a, acc) => a :: acc)

  def reverse4[A](x: List[A]): List[A] = x.foldLeft(List[A]())((acc, element) => element :: acc)


}
