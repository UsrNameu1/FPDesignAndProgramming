package laziness


sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => Empty
    case (c, Empty) => Empty
    case (c, Cons(h, t)) => Cons(h, () => t().take(c - 1))
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (c, Empty) => Empty
    case (c, Cons(h, t)) => t().drop(c - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p) )
      else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) =>
      if (!p(h())) false
      else t().forAll(p)
  }

  def takeWhileByFold(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty) { (a, acc) =>
      if (p(a)) Stream.cons(a, acc) else Empty
    }

  def headOptionByFold: Option[A] =
    foldRight[Option[A]](None) { (a, _) =>
      Some(a)
    }

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty) { (a, bs) =>
      Stream.cons(f(a), bs)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty) { (a, as) =>
      if (p(a)) Stream.cons(a, as)
      else as
    }

  def append[B >: A](bs: => Stream[B]): Stream[B] = {
    foldRight[Stream[B]](bs) { (a, acc) =>
      Cons(() => a, () => acc)
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty) { (a, bs) =>
      f(a) append bs
    }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}