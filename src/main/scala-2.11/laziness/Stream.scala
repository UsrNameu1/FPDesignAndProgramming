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

  def take(n: Int): Stream[A] = (n > 0, this) match {
    case (false, _) => Empty
    case (true, Empty) => Empty
    case (true, Cons(h, t)) => Cons(h, () => t().take(n - 1))
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

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => b && p(a) }

  def takeWhileByFold(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty) { (a, acc) =>
      if (p(a)) Stream.cons(a, acc) else Empty
    }

  def headOptionByFold: Option[A] =
    foldRight[Option[A]](None) { (a, _) =>
      Some(a)
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) { (a, bs) =>
      Stream.cons(f(a), bs)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, as) =>
      if (p(a)) Stream.cons(a, as)
      else as
    }

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B]) { (a, bs) =>
      f(a) append bs
    }

  def mapByUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeByUnfold[A](n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (Stream.empty[A], 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
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

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[BigInt] = {
    def fib(b: BigInt, bb: BigInt): Stream[BigInt] = cons(b, fib(bb, b + bb))
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def fibsByUnfold: Stream[Int] = unfold((0, 1)) {
    case (b, bb) => Some((b, (bb, bb + b)))
  }

  def fromByUnfold(n: Int): Stream[Int] =
    unfold(n) { s => Some((s, s + 1)) }

  def constantByUnfold(n: Int): Stream[Int] =
    unfold(n) { s => Some((s, s)) }

  def onesByUnfold: Stream[Int] =
    unfold(1) { s => Some((1, s)) }


}
