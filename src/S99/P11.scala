package S99

/**
 * http://aperiodic.net/phil/scala/s-99/

P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

 */
object P11 {

  def encodeModified(x: List[Symbol]): List[Any] = {
    P09.pack(x) map {
      case e if e.length == 1 => e.head
      case e => (e.length, e.head)
    }
  }

  def encodeModified2(x: List[Symbol]): List[Any] = {
    P10.encode(x) map {
      case t => if (t._1 == 1) t._2 else t
    }
  }


  def encodeModified3[A](x: List[A]): List[Either[A, (Int, A)]] = {
    P10.encode(x) map {
      case t => if (t._1 == 1) Left(t._2) else Right(t)
    }
  }

  def main(args: Array[String]) {
    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }

}
