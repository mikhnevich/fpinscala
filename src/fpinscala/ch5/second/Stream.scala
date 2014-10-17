package fpinscala.ch5.second

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // 5.1
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  // 5.1
  def toList: List[A] = {
    @tailrec
    def f(st: Stream[A], acc: List[A]): List[A] = st match {
      case Cons(h, t) => f(t(), h() :: acc)
      case _ => acc
    }
    f(this, List()).reverse
  }

  // 5.2
  // Write the function take(n) for returning the first n elements of a Stream, and
  // drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case _ => Stream()
    }
    else Stream.empty
  }

  // 5.2
  def drop(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => t()
      case Cons(h, t) => t().drop(n - 1)
      case _ => Stream()
    }
    else Stream.empty
  }

  // 5.3
  // Write the function takeWhile for returning all starting elements of a Stream that
  // match the given predicate.
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t() takeWhile f)
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 5.4
  // Implement forAll, which checks that all elements in the Stream match a given predicate.
  // Your implementation should terminate the traversal as soon as it encounters a
  // nonmatching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  // Use foldRight to implement takeWhile.
  def takeWhile_foldRight(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty)
  }

  // 5.6
  // Hard: Implement headOption using foldRight.
  def headOption_foldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  // Implement map, filter, append, and flatMap using foldRight. The append method
  // should be non-strict in its argument.
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))


  // 5.7
  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (f(a)) Stream.cons(a, b) else b.filter(f)
    )

  // 5.7
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  // 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  // 5.13
  // Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
  // zipAll. The zipAll function should continue the traversal as long as either stream
  // has more elements—it uses Option to indicate whether each stream has been
  // exhausted.

  // 5.13
  def take_unfold(n: Int): Stream[A] = ???

  // 5.13
  def takeWhile_unfold(f: A => Boolean): Stream[A] = ???

  // 5.13
  def map_unfold[B](f: A => B): Stream[B] = ???

  // 5.13
  def zipWith[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = ???

  // 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones)

  // 5.8
  // Generalize ones slightly to the function constant, which returns an infinite Stream of
  // a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  // Write a function that generates an infinite stream of integers, starting from n, then n
  // + 1, n + 2, and so on.7
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))


  // 5.10
  // Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
  // 2, 3, 5, 8, and so on.
  def fib: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] =
      cons(a, f(b, a + b))
    f(0, 1)
  }

  // 5.11
  // Write a more general stream-building function called unfold. It takes an initial state,
  // and a function for producing both the next state and the next value in the generated
  // stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) => Stream.cons(a, unfold(s)(f))} getOrElse Stream.empty[A]

  // 5.12
  def ones_unfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  // 5.12
  def constant_unfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  // 5.12
  def from_unfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  // 5.12
  def fib_unfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b))}

}

object StreamTest {
  def main(args: Array[String]) {
    println(Stream.fib take 10 toList)
  }
}