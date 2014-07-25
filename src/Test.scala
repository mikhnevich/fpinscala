import fpinscala._

object Test extends App {
  val o = Some(1)
  val n = None


  def f(i: Int) = i * 2

  val k = Ch4.lift(f)
  println(k)
  println(k(Some(2)))
}
