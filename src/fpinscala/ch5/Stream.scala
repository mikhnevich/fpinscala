package fpinscala.ch5

import scala.annotation.tailrec
import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toList: List[A] = {
    @tailrec
    def convert(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => convert(t(), h() :: acc)
      case _ => acc
    }
    convert(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 1 => this
    case Cons(h, t) => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileFoldRight(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else empty)
  }

  def headOptionFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = fibs2(0, 1)

  def fibs2(a: Int, b: Int): Stream[Int] = {
    cons(a, fibs2(b, a + b))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(s => Some(1, 1))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constantViaUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1))}

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (k, Cons(h, t)) if k == 1 => Some(h(), (k - 1, empty))
    case (k, Cons(h, t)) if k > 0 => Some(h(), (k - 1, t()))
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case (Cons(h, t)) if f(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
  }

  //  Hard: Implement startsWith using functions you’ve written. It should check if one
  //  Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
  //  would be true.
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  }

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
}