package fpinscala.ch3.second

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(_) => 1
  }

  // 3.26
  // Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  // In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  // and y.)
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27
  // Write a function depth that returns the maximum path length from the root of a tree
  // to any leaf.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  // Write a function map, analogous to the method of the same name on List, that modifies
  // each element in a tree with a given function.
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  // Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  // over their similarities. Reimplement them in terms of this more general function. Can
  // you draw an analogy between this fold function and the left and right folds for List?
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size_fold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum_fold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depth_fold[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _ max _)

  def map_fold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}

object TreeTest {
  def main(args: Array[String]) {
    val t = Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(77)
    )
    println(Tree.depth(t))
    println(Tree.depth_fold(t))
  }

}