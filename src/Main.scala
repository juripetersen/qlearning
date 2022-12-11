package src.Main

import scala.util.Random
import src.State.RNG
import src.State.RNG.{double, nonNegativeInt}

import java.security.Policy
import scala.annotation.tailrec

object Main:
  type QTable[S,A] = Map[S,Map[A,Double]]
    def table_cons[S,A](state_keys: List[S], action_keys: List[A]): QTable[S,A] =
      state_keys.map(s => (s, action_keys.map(a => (a,0.0)).toMap)).toMap

  val epsilon = 0.1
  val alpha = 0.1
  val gamma = 1.0
  trait Environment[State,Action]:
    def possible_actions(currentState: State): List[Action]
    def step(currentState:State, action_taken:Action): (State,Double)
    def isTerminal(state:State): Boolean

  def epoch[S,A](state : S, q_table : QTable[S,A], rng : RNG, state_apply_action : (S,A) => (S,Double)): (S, QTable[S,A], RNG) =
    val (policy, new_rng)   = q_table.policy(rng.nextInt._2) //generates new RNG
    val chosen_action       = policy(state)
    //println("Moving " + chosen_action + " from " + state)
    val (new_state, reward) = state_apply_action(state,chosen_action)
    //println("Policy " + policy)
    val chosen_action_*     = policy(new_state)
    val current_action_val  = q_table(state)(chosen_action)
    val new_action_val      = q_table(new_state)(chosen_action_*)
    val newReward           = current_action_val + alpha * (reward + gamma * new_action_val - current_action_val) //is this correct?
    val updated_reward_map  = q_table(state).updated(chosen_action, newReward)
    val newQTable           = q_table.updated(state,updated_reward_map)

    (new_state, newQTable, new_rng.nextInt._2) //should pass on a new rng

  @tailrec
  def episode[S,A](state : S, environment: Environment[S,A], q_table : QTable[S,A], rng : RNG, state_apply_action : (S,A) => (S,Double)) : QTable[S,A] =
    val epoch_val   = epoch(state,q_table,rng.nextInt._2,state_apply_action) //should pass on a new RNG
    val new_state   = epoch_val._1
    val new_q_table = epoch_val._2
    if environment.isTerminal(new_state) then new_q_table else episode(new_state,environment,new_q_table, epoch_val._3.nextInt._2, state_apply_action) //should pass on a new RNG


  type Policy[S,A] = Map[S,A]
    extension[S, A](q_table: QTable[S, A])
      def policy(rng : RNG): (Policy[S, A], RNG) =
        val size = q_table.size-1

        @tailrec
        def step(count: Int, r: RNG, policy: Policy[S,A]): (Policy[S,A], RNG) =
          if count < 0 then (policy, r.nextInt._2)
          else
            val (v: Double, new_rng: RNG)   = double(r);
            val (v2: Double, new_rng2: RNG) = double(new_rng)
            val cur_state                   = q_table.keys.toList(count)
            val actions                     = q_table(cur_state)

            step(count - 1, new_rng2.nextInt._2, policy + (q_table.keys.toList(count) -> epsilon_greedy(v,v2,actions,epsilon)))
        step(size, rng.nextInt._2, Map.empty[S,A])

    def epsilon_greedy[S,A](randDouble: Double, randDouble2: Double, actions: Map[A,Double], epsilon: Double): A =
      if (randDouble < epsilon)
        actions.keys.toList((randDouble2 * actions.size).toInt % actions.size)
      else
        actions.toList.maxBy(_._2)._1



