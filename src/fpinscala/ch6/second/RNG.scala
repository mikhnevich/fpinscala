package fpinscala.ch6.second

trait RNG {
  def nextInt: (Int, RNG)
}
