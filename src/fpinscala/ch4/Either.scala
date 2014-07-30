package fpinscala.ch4

import fpinscala.ch3
import fpinscala.ch3.Cons

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  //map(f) getOrElse None
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  //    flatMap(x => b.map(y => f(x, y)))
    for {a <- this; b1 <- b} yield f(a, b1)

  def sequence[E, A](es: ch3.List[Either[E, A]]): Either[E, ch3.List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: ch3.List[A])(f: A => Either[E, B]): Either[E, ch3.List[B]] = as match {
    case ch3.Nil => Right(ch3.Nil)
    case Cons(h, t) => (f(h) map2 traverse(t)(f))(Cons(_, _))
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
}

