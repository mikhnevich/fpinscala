package test

/**
 * Created by siarheim on 7/23/14.
 */
class Complex(val re: Int = 0, im: Int = 0) extends Ordered[Complex] {
  override def compare(that: Complex): Int = this.re.compareTo(that.re)
}
