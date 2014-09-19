package S99

/**
 * http://aperiodic.net/phil/scala/s-99/

P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

 */
object P09 {

  def pack[A](x: List[A]): List[List[A]] = x match {
    case Nil => List()
    case h :: tail =>
      val a = List(h :: tail.takeWhile(_ == h))
      val b = pack(tail.dropWhile(_ == h))
      a ++ b
  }

  def pack2[A](x: List[A]): List[List[A]] = x match {
    case Nil => List()
    case h :: tail =>
      val (packed, next) = x span (_ == h)
      packed :: pack(next)
  }



  def main(args: Array[String]) {
    println(pack2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }

}
