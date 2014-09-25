package fpinscala.ch2

import scala.annotation.tailrec


object Test {
  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(index: Int, prev: A): Boolean = {
      if (index == as.length) true
      else {
        if (ordered(prev, as(index))) go(index + 1, as(index)) else false
      }
    }
    if (as != null && as.length > 0) go(1, as(0)) else false
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // 2.3
  def curry[A, B, C](f:(A, B) => C): A => (B => C) = a => b => f(a, b)

  // 2.4
  def uncurry[A, B, C](f: A => B)(g: B => C): (A, B) => C = (a, b) => g(f(a))

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = f compose g // a=> f(g(a))


  def main(args: Array[String]) {
    val predicate = (a: Int, b: Int) => a < b
    println(isSorted(Array(1, 2, 3, 4, 5), predicate))
    println(isSorted(Array(1, 2, 3, 4, 3), predicate))
  }
}
