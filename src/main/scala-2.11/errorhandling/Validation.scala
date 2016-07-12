package errorhandling


sealed trait Validation[+E, +A] {

  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(a) => Success(f(a))
    case fa @ Failure(_) => fa
  }

  def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B] = this match {
    case s @ Success(_) => s
    case Failure(e) => b match {
      case s @ Success(_) => s
      case Failure(ee) => Failure(e ++ ee)
    }
  }

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = (this, b) match {
    case (Success(aa), Success(bb)) => Success(f(aa, bb))
    case (fa @ Failure(_), Success(_)) => fa
    case (Success(_), fb @ Failure(_)) => fb
    case (Failure(ea), Failure(eb)) => Failure(ea ++ eb)
  }

}

case class Failure[+E](value: List[E]) extends Validation[E, Nothing]
case class Success[+A](value: A) extends Validation[Nothing, A]

object Validation {

  def sequence[E, A](es: List[Validation[E, A]]): Validation[E, List[A]] =
    es.foldRight[Validation[E, List[A]]](Success(Nil)) { (v, acc) =>
      v.map2(acc)(_ :: _)
    }

  def traverse[E, A, B](as: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
    as.foldRight[Validation[E, List[B]]](Success(Nil)) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }
}