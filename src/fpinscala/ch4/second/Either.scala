package fpinscala.ch4.second


sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)

  // 4.6
  // Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  // 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  // 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }


  // 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)


}


object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int) = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try {
        age.toInt
      }
      tickets <- Try {
        numberOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, tickets)

  // 4.7
  // Implement sequence and traverse for Either. These should return the first error
  // that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]