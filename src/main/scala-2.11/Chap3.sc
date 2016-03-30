import fpinscala.datastructures._
import fpinscala.datastructures.List._

val v = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
 }

tail(List(1, 2, 3))
setHead(List(1, 3, 4), 2)
drop(List(1, 2, 3, 4), 1)
drop(List(1, 2, 3, 4), 2)
drop(List(1, 2, 3, 4), 4)
init(List(1, 2, 3, 4))

// 3.7 impossible for reason of the page 49
// 3.8 catamorphism
// (see also:
//    http://dev.classmethod.jp/server-side/scala-differece-between-fold-and-foldleft/
//    http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf
// )

length(List(1, 2, 3))
foldLeft(List(1, 2, 3), 0) ( _ + _ )

def foldSum(ls: List[Int]): Int =
  foldLeft(ls, 0) ( _ + _ )

foldSum(List(1, 4, 5))

def foldProduct(ls: List[Int]): Int =
  foldLeft(ls, 1) ( _ * _ )

foldProduct(List(1, 4, 5, 3))

def foldLength(ls: List[Int]): Int =
  foldLeft(ls, 0) ( (acc, _) => acc + 1 )

foldLength(List(2, 3, 4))

reverse(List(1, 2, 3))

def foldReverse[A](as: List[A]): List[A] =
  foldRight[A, List[A]](as, Nil) ( (a, acc) => append(acc, List(a)))

foldReverse(List(1, 2, 3))

// skip 3.13 for a while

def foldAppend[A](as: List[A], bs: List[A]): List[A] =
  foldRight(as, bs) ( (a, acc) => Cons(a, acc) )

foldAppend(List(1, 2, 3), List(4, 5, 6))

concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

def mapAddOne(is: List[Int]): List[Int] =
  map(is) { _ + 1 }

mapAddOne(List(1, 3, 4))

def mapToString(ds: List[Double]): List[String] =
  map(ds) { _.toString }

map(List(1, 2, 4)) { _.toString }

filter(List(1, 2, 3, 4, 5)) { _ % 2 == 0 }
