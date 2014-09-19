package S99

/**
 * http://aperiodic.net/phil/scala/s-99/

Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

 */
object P08 {

  def compress(x: List[Symbol]): List[Symbol] = x match {
    case Nil => Nil
    case a :: b :: t if a == b => compress(b :: t)
    case a :: b :: t if a != b => List(a) ++ compress(b :: t)
    case a :: Nil => List(a)
  }

  def compress2(x: List[Symbol]): List[Symbol] = x match {
    case Nil => Nil
    case h :: t => h:: compress2(t.dropWhile(_ == h))
  }


  def main(args: Array[String]) {
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(compress2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }

}
