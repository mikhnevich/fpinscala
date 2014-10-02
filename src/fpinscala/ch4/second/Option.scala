package fpinscala.ch4.second


sealed trait Option[+A] {
  // 4.1
  // Implement all of the preceding functions on Option. As you implement each function,
  // try to think about what it means and in what situations you’d use it. We’ll explore when
  // to use each of these functions next. Here are a few hints for solving this exercise:
  // It’s fine to use pattern matching, though you should be able to implement all
  // the functions besides map and getOrElse without resorting to pattern matching.
  // For map and flatMap, the type signature should be enough to determine the
  // implementation.
  // getOrElse returns the result inside the Some case of the Option, or if the Option
  // is None, returns the given default value.
  // orElse returns the first Option if it’s defined; otherwise, it returns the second
  // Option.

  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  // 4.1
  def flatMap[B](f: A => Option[B]): Option[B] =

  // 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  // 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] =

  // 4.1
  def filter(f: A => Boolean): Option[A] = ???
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]