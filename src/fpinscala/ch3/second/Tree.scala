package fpinscala.ch3.second

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](t: Tree[A]): Int =
    t match {
      case b: Branch[A] => size(b.left) + size(b.right) + 1
      case _ => 1
    }

  // 3.26
  // Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  // In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  // and y.)

  // 3.27
  // Write a function depth that returns the maximum path length from the root of a tree
  // to any leaf.

  // 3.28
  // Write a function map, analogous to the method of the same name on List, that modifies
  // each element in a tree with a given function.
}

object TreeTest {
  def main(args: Array[String]) {
    val t = Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(77)
    )
    println(Tree.size(t))
  }

}