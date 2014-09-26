package fpinscala.ch3.second

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  // empty list => 1 ???
  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("empty list")
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => sys.error("empty list")
    case Cons(_, tail) => Cons(h, tail)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case _ if n == 0 => l
      case Cons(h, t) => drop(t, n - 1)
    }

  // 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Nil => sys.error("empty list")
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(xs: List[Int]) = {
    foldRight(xs, 0)(_ + _)
  }

  def product2(xs: List[Int]) = {
    foldRight(xs, 1)(_ * _)
  }

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, sum) => sum + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) =>
      println(h)
      foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sum_foldLeft(xs: List[Int]) = foldLeft(xs, 0)(_ + _)

  // 3.11
  def product_foldLeft(xs: List[Int]) = foldLeft(xs, 1)(_ * _)

  // 3.11
  def length_foldLeft(xs: List[Int]) = foldLeft(xs, 0)((acc, _) => acc + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, elem) => Cons(elem, acc))

  //  3.13
  def foldLeft_[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // 3.13
  def foldRight_[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => g compose (b => f(a, b)))(z)

  // 3.14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  // 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  // 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((elem, acc) => Cons(elem + 1, acc))

  // 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // 3.19
  // Write a function filter that removes elements from a list unless they satisfy a given
  // predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // 3.20
  //  Write a function flatMap that works like map except that the function given will return
  //  a list instead of a single result, and that list should be inserted into the final resulting
  //  list. Here is its signature:
  //  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
  //  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  //  List(1,1,2,2,3,3).
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  //    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  // 3.21
  // Use flatMap to implement filter.
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  // Write a function that accepts two lists and constructs a new list by adding corresponding
  // elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  // 3.23
  // Generalize the function you just wrote so that it’s not specific to integers or addition.
  // Name your generalized function zipWith.
  def zipWith[A, B](a: List[A], b: List[B]): List[(A, B)] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zipWith(t1, t2))
  }

  // 3.24
  // Hard: As an example, implement hasSubsequence for checking whether a List contains
  // another List as a subsequence. For instance, List(1,2,3,4) would have
  // List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  // some difficulty finding a concise purely functional implementation that is also efficient.
  // That’s okay. Implement the function however comes most naturally. We’ll
  // return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  // two values x and y can be compared for equality in Scala using the expression x == y.
  // def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(main: List[A], l: List[A]): Boolean =
      (main, l) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (h1 == h2) startsWith(t1, t2)
          else startsWith(t1, sub)
      }
    startsWith(sup, sub)
  }
}

object ListTest {
  def main(args: Array[String]): Unit = {
    //    println(List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
    //    println(List.addOne(List(1, 2, 3)))
    //    println(List.addOne(List(1)))
    //    println(List.addOne(Nil))
    //    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 3))) // F
    println(List.hasSubsequence(List(1, 2, 3, 4), List(4, 5, 6))) // F
    println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))) // T
    println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))) // T
    println(List.hasSubsequence(List(1, 2, 3, 4), List(4))) // T
  }
}