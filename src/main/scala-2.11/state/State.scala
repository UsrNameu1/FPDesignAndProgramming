package state

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, sa) = run(s)
    (f(a), sa)
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, sa) = run(s)
    g(a).run(sa)
  }

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, sta) = sa.run(s)
      val (b, stb) = sb.run(sta)
      (f(a, b), stb)
    }

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight[State[S, List[A]]](unit(Nil)) { case (state, acc) =>
      State { s =>
        val (a, sa) = state.run(s)
        val (as, sas) = acc.run(sa)
        (a::as, sas)
      }
    }
}