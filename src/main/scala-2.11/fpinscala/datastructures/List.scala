package fpinscala.datastructures

import annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match { // possibly be defined in Option[A+]
    case Nil => Nil
    case Cons(a, as) => as
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def setHead[A](xs: List[A], other: A): List[A] = xs match {
    case Nil => Nil
    case Cons(_, as) => Cons(other, as)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(ls: List[A], count: Int): List[A] = count match {
      case 0 => ls
      case c => go(tail(ls), c - 1)
    }
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def go(ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(a, as) if f(a) => go(as)
    }
    go(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(ls: List[A], acc: B): B = ls match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }
    go(as, z)
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0) ( (_, acc) => acc + 1 )

  def append[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil => bs
    case Cons(x, xs) => Cons(x, append(xs, bs))
  }

  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(reverse(xs), apply(x))
  }

  def concat[A](ass: List[List[A]]): List[A] =
    foldRight(ass, Nil: List[A]) (append)

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def exists[A](as: List[A])(f: A => Boolean): Boolean = as match { // redefine for performance problem
    case Nil => false
    case Cons(x, xs) => f(x) || exists(xs)(f)
  }

  def contains[A](as: List[A])(a: A): Boolean =
    exists(as)(_ == a)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    foldRight(sub, true)( (a, acc) => acc && contains(sup)(a))
}
