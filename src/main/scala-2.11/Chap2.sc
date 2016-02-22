import scala.annotation.tailrec

def fib(index: Int): Int = {
  @tailrec
  def go(bb: Int, b: Int, idx: Int): Int = idx match {
    case 1 => bb
    case 2 => b
    case i => go(b, bb + b, i - 1)
  }
  go(0, 1, index)
}

(1 to 10).map(fib)

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @tailrec
  def loop(idx: Int): Boolean = idx match {
    case 0 if as.length <= 1 => true
    case n if as.length - 2 <= n => ordered(as(n), as(n + 1))
    case n => if (!ordered(as(n), as(n + 1))) false else loop(n + 1)
  }
  loop(0)
}

isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a > b )
isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b )
isSorted(Array(), (a: Int, b: Int) => a > b )
isSorted(Array(1), (a: Int, b: Int) => a > b )

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

val curryedAdd = curry { (a: Int, b: Int) => a + b }
val add1 = curryedAdd(1)
add1(2)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

val add = uncurry(curryedAdd)
add(1, 2)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val composed = compose[Int, Int, Int]( a => a + 1, b => b + 2 )
composed(4)
