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
    foldRight[Stream[B]](Empty) { (a, bs) =>
      Stream.cons(f(a), bs)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty) { (a, as) =>
      if (p(a)) Stream.cons(a, as)
      else as
    }

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](bs) { (a, acc) =>
      Cons(() => a, () => acc)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty) { (a, bs) =>
      f(a) append bs
    }

  def mapByUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeByUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (_, 0) => None
      case (Empty, _) => None
      case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
    }

  def takeWhileByUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithByUnfold[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).forAll {
      case (None, Some(a)) => false
      case (Some(a), None) => true
      case (Some(a), Some(b)) => a == b
    }

  def tails: Stream[Stream[A]] =
    Stream.cons(this, Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((t(), t()))
    })

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h, t) =>
      val acc@Cons(acch, _) = t().scanRight(z)(f)
      Stream.cons(f(h(), acch()), acc)
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

  def fibs: Stream[Int] = {
    def fib(b: Int, bb: Int): Stream[Int] = cons(b, fib(bb, b + bb))
    cons(0, fib(1, 1))
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
