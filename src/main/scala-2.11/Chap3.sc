import fpinscala.datastructures._
import fpinscala.datastructures.List._
import fpinscala.datastructures.Tree.{map => tmap, _}

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

flatMap(List(1, 2, 3))(i => List(i, i))

def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as) { a => if (f(a)) List(a) else Nil }

filterByFlatMap(List(1, 2, 3))(a => a % 2 == 1)

zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)

hasSubsequence(List(1, 2, 3, 4), List(1, 2))

size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))
tmap(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ * 2)

def sizeByFold[A](tree: Tree[A]): Int =
  fold[A, Int](tree, _ => 1 )( (lacc, racc) => 1 + lacc + racc)

sizeByFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))

def maximumByFold(tree: Tree[Int]): Int =
  fold[Int, Int](tree, elem => elem )( (lacc, racc) => lacc max racc )

maximumByFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))

def depthByFold[A](tree: Tree[A]): Int =
  fold[A, Int](tree, _ => 0 )( (lacc, racc) => 1 + (lacc max racc) )

depthByFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))

def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
  fold[A, Tree[B]](tree, elem => Leaf(f(elem)))( (lacc, racc) => Branch(lacc, racc))

mapByFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ * 2)