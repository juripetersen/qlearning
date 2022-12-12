package adpro.rl

import scala.util.Random
//import src.State.RNG
import adpro.rl.RNG.{double, nonNegativeInt}

import java.security.Policy
import scala.annotation.tailrec

trait Environment[S,A]:
  def step(currentState: S, actionTaken: A): (S,Double)
  def isTerminal(state: S): Boolean

object QLearning:
  type QTable[S,A] = Map[S,Map[A,Double]]

  object QTable:
    def apply[S,A](states: List[S], actions: List[A]): QTable[S,A] =
      states.map(s => (s, actions.map(a => (a, 0.0)).toMap)).toMap

  end QTable

  val epsilon = 0.1
  val alpha = 0.1
  val gamma = 1.0


  def epoch[S,A](state : S, qtable : QTable[S,A], rng : RNG, stateApplyAction : (S,A) => (S,Double)): (S, QTable[S,A], RNG) =
    val (policy, newRng)   = qtable.policy(rng.nextInt._2)
    val chosenAction       = policy(state)
    val (newState, reward) = stateApplyAction(state, chosenAction)
    val chosenNextAction     = policy(newState)
    val currentActionValue  = qtable(state)(chosenAction)
    val nextActionValue      = qtable(newState)(chosenNextAction)
    val newReward           = currentActionValue + alpha * (reward + gamma * nextActionValue - currentActionValue)
    val updatedRewardMap  = qtable(state).updated(chosenAction, newReward)
    val newQTable           = qtable.updated(state, updatedRewardMap)

    (newState, newQTable, newRng.nextInt._2)

  @tailrec
  def episode[S,A](state : S, environment: Environment[S,A], qtable : QTable[S,A], rng : RNG, stateApplyAction : (S,A) => (S,Double)) : QTable[S,A] =
    val (newState, newQTable, newRng) = epoch(state, qtable, rng.nextInt._2,stateApplyAction)
    if environment.isTerminal(newState) then
      newQTable
    else episode(newState, environment, newQTable, newRng.nextInt._2, stateApplyAction)


  type Policy[S,A] = Map[S,A]
    extension[S, A](qtable: QTable[S, A])
      def policy(rng : RNG): (Policy[S, A], RNG) =
        val size = qtable.size-1

        @tailrec
        def step(count: Int, r: RNG, policy: Policy[S,A]): (Policy[S,A], RNG) =
          if count < 0 then (policy, r.nextInt._2)
          else
            val (v: Double, newRng: RNG)   = double(r)
            val (v2: Double, newRng2: RNG) = double(newRng)
            val currentState = qtable.keys.toList(count)
            val actions = qtable(currentState)

            step(count - 1, newRng2.nextInt._2, policy + (qtable.keys.toList(count) -> epsilonGreedy(v,v2,actions,epsilon)))
        step(size, rng.nextInt._2, Map.empty[S,A])

    def epsilonGreedy[S,A](randDouble: Double, randDouble2: Double, actions: Map[A,Double], epsilon: Double): A =
      //println("rng: " + randDouble + " " + randDouble2)
      if (randDouble < epsilon)
        actions.keys.toList((randDouble2 * actions.size).toInt % actions.size)
      else
        actions.toList.maxBy(_._2)._1
