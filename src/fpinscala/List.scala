package fpinscala

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]) = l match {
    case Nil => sys.error("tail on empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](h: A, l: List[A]) = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (0, xs) => xs
    case (_, Nil) => Nil
    case _ => drop(l, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
    case _ => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => 1 + length(t)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFL(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]) = foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((b, a) => Cons(a, b))

  // http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, identity[B] _)((a, g) => g compose (b => f(b, a)))(z)

  //    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldLeftViaFoldRight2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def h(a: A, g: B => B): (B => B) = g compose ((x: B) => f(x, a))
    foldRight(l, identity[B] _)(h)(z)
  }

  def append2[A](l: List[A], r: List[A]) = foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append2)

  def plusOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, plusOne(xs))
  }

  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String]())((e, acc) => Cons(e.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filter_via_flatmap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else List())

  def addTwoLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addTwoLists(xs, ys))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) =>
      if (x == y) {
        val success = hasSubsequence(xs, ys)
        if (success) true else hasSubsequence(xs, sub)
      } else {
        hasSubsequence(xs, sub)
      }
  }

}

