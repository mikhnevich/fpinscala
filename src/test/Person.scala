package test

class Person (var age: Int) extends Ordered[Person] {
  override def compare(that: Person): Int = that.age.compareTo(age)
}

