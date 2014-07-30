import fpinscala._
import fpinscala.ch3.{Cons, Nil}
import fpinscala.ch4.Option

object Test extends App {
  val o = ch4.Some(1)
  val n = ch4.None


  def f(i: Int) = i * 2
//  val list = Cons(Some(1), Cons(Some(2), Nil))
//  val list = Nil
  val list = Cons(ch4.Some(1), Cons(ch4.None, Nil))
  val opt = Option.sequence(list)
  println(opt)
}
