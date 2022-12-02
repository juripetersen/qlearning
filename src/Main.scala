import scala.util.Random
object Main:


  opaque type QTable[S,A] = Map[S,Map[A,Double]]
    def table_cons[S,A](state_keys: List[S], action_keys: List[A]): QTable[S,A] =
      state_keys.map(s => (s, action_keys.map(a => (a,0.0)).toMap)).toMap

  def main(args: Array[String]): Unit =
    val alpha = 0
    val epsilon = 0
    val gamma = 0

  case class State(x : Int, y: Int)
  val random = 0
  val epsilon = 1


  type Policy[S,A] = Map[S,A]
    def chooseAction[S,A](state: S, actions: Map[A, Double], chooser: (S, Map[A,Double]) => A): A =
      chooser(state,actions)

      /*
      if (random < epsilon) {

        Random.shuffle(actions.keys.toList).head
      } else {
        actions.toList.maxBy(_._2)._1
      }
      */

    def make_policy_table[S,A](states: List[S], actions: Map[A, Double]): Unit =
      states.map(s => (s, chooseAction(s, actions)))




  def Q[S,A](state: S, list: A): QTable[S,A] = ???



