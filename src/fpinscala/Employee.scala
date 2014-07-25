package fpinscala

case class Employee(name: String, department: String) {

  def lookupByName (name: String): Option[Employee] = ???

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
}

