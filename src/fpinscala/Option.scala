package fpinscala

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case (None, Nil) => None
    case (Some(x), Nil) => Some(List(x))
    case (None, t) => None[List[A]]
    case (Some(x), t) =>
      val v = sequence(t).flatMap()
      v.flatMap()

  }
}