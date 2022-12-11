//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using lib "dev.optics::monocle-core:3.1.0"
//> using lib "dev.optics::monocle-macro:3.1.0"

package src

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import src.CliffWalking.*
import src.Main.*

object QLearningSpec
  extends org.scalacheck.Properties("QLearning"):

  property("The selection of best action (as part of" +
    " the ε-greedy selection) always returns the highest value action for the state") =
    val map: Map[CliffWalking.Move, Double] = Map(
      CliffWalking.Move.Up -> 0,
      CliffWalking.Move.Down -> -10,
      CliffWalking.Move.Left -> -10,
      CliffWalking.Move.Right -> -1.90,
      )

    forAll(Gen.double, Gen.double) {
      (randDouble: Double, randDoubleTwo: Double) =>
        Main.epsilon_greedy(randDouble, randDoubleTwo, map, 0.0) == map.toList.maxBy(_._2)._1
    }


  property("The selection of best action (as part of" +
    " the ε-greedy selection) always returns the highest value action for the state") =
    val epsilon = 0.5
    val map: Map[CliffWalking.Move, Double] = Map(
      CliffWalking.Move.Up -> 0,
      CliffWalking.Move.Down -> -10,
      CliffWalking.Move.Left -> -10,
      CliffWalking.Move.Right -> -1.90,
      )

    forAll(Gen.double, Gen.double) {
      (randDouble: Double, randDoubleTwo: Double) =>
        Main.epsilon_greedy(randDouble, randDoubleTwo, map, 0.0) == map.toList.maxBy(_._2)._1
    }
