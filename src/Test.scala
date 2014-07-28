import fpinscala._

object Test extends App {
  val o = Some(1)
  val n = None


  def f(i: Int) = i * 2
//  val list = Cons(Some(1), Cons(Some(2), Nil))
//  val list = Nil
  val list = Cons(Some(1), Cons(None, Nil))
  val opt = Option.sequence(list)
  println(opt)
}
