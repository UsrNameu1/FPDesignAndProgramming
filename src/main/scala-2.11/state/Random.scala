package state


trait RNG {
  def nextInt: (Int, RNG)
}

object Random {

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State { _.nextInt }

  def unit[A](a: A): Rand[A] = State { rng => (a, rng) }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = State { rng =>
    val (a, rng2) = s.run(rng)
    (f(a), rng2)
  }

  def double2: Rand[Double] = map(int) { randInt =>
    val nonNegInt = randInt match {
      case Int.MinValue => 0
      case i => math.abs(i)
    }
    nonNegInt / Int.MaxValue
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State { rng =>
    val (a, rnga) = ra.run(rng)
    val (b, rngb) = rb.run(rnga)
    (f(a, b), rngb)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil)) { case (rand, acc) =>
      State { rng =>
        val (a, rnga) = rand.run(rng)
        val (as, rngas) = acc.run(rnga)
        (a::as, rngas)
      }
    }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State { rng =>
    val (a, rnga) = f.run(rng)
    g(a).run(rnga)
  }

  def nonNegativeLessThen(n: Int): Rand[Int] =
    flatMap(State(nonNegativeInt)) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) State{ (mod, _) }
      else nonNegativeLessThen(n)
    }

  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, random) = rng.nextInt
    val retInt = randomInt match {
      case Int.MinValue => 0
      case i => math.abs(i)
    }
    (retInt, random)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nonNegative, random) = nonNegativeInt(rng)
    (nonNegative / Int.MaxValue.toDouble, random)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randomInt, firstRng) = nonNegativeInt(rng)
    val (randomDouble, secondRng) = double(firstRng)
    ((randomInt, randomDouble), secondRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (randomDouble, firstRng) = double(rng)
    val (randomInt, secondRng) = nonNegativeInt(firstRng)
    ((randomDouble, randomInt), secondRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (firstDouble, firstRng) = double(rng)
    val (secondDouble, secondRng) = double(firstRng)
    val (thirdDouble, thirdRng) = double(secondRng)
    ((firstDouble, secondDouble, thirdDouble), thirdRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case n =>
      val (tail, random) = ints(n - 1)(rng)
      val (head, nextRandom) = random.nextInt
      (head::tail, nextRandom)
  }
}
