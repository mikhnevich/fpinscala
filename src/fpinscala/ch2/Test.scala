package fpinscala.ch2


object Test {
  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as == null || as.length <= 1) {
      true
    } else {
      as.foldLeft((true, as(0))){case (b, a) => if (b._1) (ordered(b._2, a), a) else (false, a)}._1
    }
  }

}
