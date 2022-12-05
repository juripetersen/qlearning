package src


import scala.util.Random

import src.State.RNG

object Main:


  opaque type QTable[S,A] = Map[S,Map[A,Double]]
    def table_cons[S,A](state_keys: List[S], action_keys: List[A]): QTable[S,A] =
      state_keys.map(s => (s, action_keys.map(a => (a,0.0)).toMap)).toMap
    
  def main(args: Array[String]): Unit = ???

  val epsilon = 0

  def epoch[S,A](state : S, qtable : QTable[S,A], rng : RNG, state_apply_action : (S,A) => (S,Double)): (S, QTable[S,A]) =
    val optimal_action     = qtable.policy(rng)._2.get(state).get
    val (newState, reward) = state_apply_action(state,optimal_action)
    val updated_reward_map = qtable.get(state).get.updated(optimal_action, reward)
    val newQTable          = qtable.updated(state,updated_reward_map)
    (newState, newQTable)

  type Policy[S,A] = (RNG, Map[S,A])
    extension[S, A](qtable: QTable[S, A])
      def policy(rng : RNG): Policy[S, A] =
        (rng.nextInt._2, qtable.map { (s, m) => (s, epsilon_greedy(rng, m, epsilon)._1) }.toMap)

    def epsilon_greedy[S,A](rng : RNG, actions: Map[A,Double], epsilon: Double): (A,RNG) =
      val randInt = rng.nextInt
      if (randInt._1 < epsilon) then (actions.keys.toList((RNG.double(rng)._1/Double.MaxValue * actions.keys.toList.length).toInt),randInt._2)
      else (actions.toList.maxBy(_._2)._1,randInt._2)


  def Q[S,A](state: S, list: A): QTable[S,A] = ???




