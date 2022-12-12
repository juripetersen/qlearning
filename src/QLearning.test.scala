//> using target { scope "test" }
//> using scala "3.1.3"
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

  property("The selection of best action (as part of" +
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


  property("The selection of best action (as part of" +
    " the ε-greedy selection) always returns the highest value action for the state") =
    val epsilon = 0.5
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
