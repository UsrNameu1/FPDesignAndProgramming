package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(b) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A], lf: A => B)(bf: (B, B) => B): B = tree match {
    case Leaf(a) => lf(a)
    case Branch(left, right) => bf(fold(left, lf)(bf), fold(right, lf)(bf))
  } // this method is similar with List foldRight when you replace lf with z and Leaf with Nil
}