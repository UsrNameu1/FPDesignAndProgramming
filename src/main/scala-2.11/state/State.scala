package state


trait RNG {
  def nextInt: (Int, RNG)
}


object State {

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
