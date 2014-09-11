package S99

import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/

P06 (*) Find out whether a list is a palindrome.
Example:
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true

 */
object P06 {

  def isPalindrome2[A](x: List[A]): Boolean = x == x.reverse

  def isPalindrome[A](x: List[A]): Boolean = {
    val len = x.length
    val first = x.take(len / 2)
    val last = x.takeRight(len / 2).reverse
    first == last
  }


  def main(args: Array[String]) {
    println(P06.isPalindrome("abccba".toList))
    println(P06.isPalindrome("abcXcba".toList))
    println(P06.isPalindrome("abcXYcba".toList))
  }

}
