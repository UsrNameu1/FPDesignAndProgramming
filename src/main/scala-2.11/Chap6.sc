import state._

val rng = Random.SimpleRNG(1)
Random.ints(6)(rng)
Random.sequence(List(Random.unit(1), Random.unit(2))).run(rng)
Random.ints2(6).run(rng)
Simulator.simulateMachine(List.fill(4)(List(Coin, Turn)).flatten).run(Machine(true, 5, 10))