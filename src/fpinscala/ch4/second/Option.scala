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
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  // 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  // 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  // 4.1
  def filter2(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)

  // 4.2
  // Implement the variance function in terms of flatMap. If the mean of a sequence is m,
  // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  // See the definition of variance on Wikipedia (http://en.wikipedia.org/wiki/Variance#Definition).
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Option.Try(age.toInt)
    val optTickets: Option[Int] = Option.Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }


}

object Option {


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case ex: Exception => None
    }

  // 4.3
  // Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (x =>
      b map (y =>
        f(x, y)))

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  // 4.4
  // Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some
  // with a list of all the values. Here is its signature:3
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def f(xs: List[Option[A]], acc: List[A]): Option[List[A]] = xs match {
      case Nil => Some(acc.reverse)
      case None :: t => None
      case Some(e) :: t => f(t, e :: acc)
    }
    f(a, List[A]())
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((elem, acc) => map2(elem, acc)(_ :: _))


  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }


  // 4.5
  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((elem, acc) => map2(f(elem), acc)(_ :: _))

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object OptionTest {
  def main(args: Array[String]) {
    val f = (a: Int) => a * 2
    println(Some(2).map(f))
    println(None.map(f))

    val g = (a: Int) => Some(a * 3)
    println(Some(2).flatMap(g))
    println(None.flatMap(g))

    println(Some(4).getOrElse(5))
    println(None.getOrElse(5))

    println(Some(6).orElse(Some(7)))
    println(None.orElse(Some(8)))
    println(None.orElse(None))
  }
}