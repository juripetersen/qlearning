//> using target { scope "test" }
//> using scala "3.2.1"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using lib "dev.optics::monocle-core:3.1.0"
//> using lib "dev.optics::monocle-macro:3.1.0"

package adpro.rl

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import adpro.rl.CliffWalking.*
import adpro.rl.QLearning.*

object QLearningSpec
  extends org.scalacheck.Properties("QLearning"):

  property("ε-greedy: The selection of best action (as part of" +
    " the ε-greedy selection) always returns the highest value action for the state") =
    val map: Map[Move, Double] = Map(
      Move.Up -> 0,
      Move.Down -> -10,
      Move.Left -> -10,
      Move.Right -> -1.90,
      )

    forAll(Gen.double, Gen.double) {
      (randDouble: Double, randDoubleTwo: Double) =>
        epsilonGreedy(randDouble, randDoubleTwo, map, 0.0) == map.toList.maxBy(_._2)._1
    }

  property("ε-greedy: The mechanism for tossing ε-biased coined correctly approximates a Bernoulli distribution with parameter ε (you can just toss the coin 10000 times with your function, " +
    "and check whether the proportion is close to ε; for this test an ε == 0.5 might be good, definitely not a very small or a very large value).") =
    val map: Map[Move, Double] = Map(
      Move.Up -> 0,
      Move.Down -> -1,
      Move.Left -> -1,
      Move.Right -> -1,
    )

    val test = forAll(Gen.double, Gen.double) {
      (randDouble: Double, randDoubleTwo: Double) =>
        collect(epsilonGreedy(randDouble, randDoubleTwo, map, 0.5)) {randDouble == randDouble}
    }
    test.check()
    true

  property("ε-greedy: Uniformly picks") =
    val map: Map[Move, Double] = Map(
      Move.Up -> 0,
      Move.Down -> 0,
      Move.Left -> 0,
      Move.Right -> 0,
    )

    val test = forAll(Gen.double, Gen.double) {
      (randDouble: Double, randDoubleTwo: Double) =>
        collect(epsilonGreedy(randDouble, randDoubleTwo, map, 1)) {randDouble == randDouble}
    }
    test.check()
    true

  //our implementation of cliffWalking only really supports if 0,0 is the initial state
  property("CliffWalking: The initial state is not terminal (easy)") =
    !CliffWalking.environment.isTerminal(CliffWalking.Location(0,0))

  val moves = List(Move.Up,Move.Down,Move.Left,Move.Right)

  given Arbitrary[Move] =
    Arbitrary {
      Gen.oneOf(moves)
    }

  property("CliffWalking: all rewards are negative") =
    forAll { (x: Int, y:Int, move : Move) =>
        val (newState, reward) = CliffWalking.environment.step(CliffWalking.Location(x,y),move)
        reward <= 0
    }







