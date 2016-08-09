package state

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Simulator {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
    val output = inputs.foldLeft(machine) {
      case (Machine(true, candies, coins), Coin) if candies > 0 =>
        Machine(locked = false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) =>
        Machine(locked = true, candies - 1, coins)
      case (m@Machine(true, _, _), Turn) => m
      case (m@Machine(false, _, _), Coin) => m
      case (m@Machine(_, 0, coins), _) => m
    }
    ((output.coins, output.candies), output)
  }

}