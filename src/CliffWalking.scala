package src
import src.Main.*
import src.State.RNG

import scala.annotation.tailrec


object CliffWalking:

  case class Location(x: Int ,y: Int)

  enum Move:
    case Up,Down,Right,Left

  implicit val environment : Environment[Location,Move] = new Environment[Location,Move]:
    def isTerminal(state:Location): Boolean = state match
      case Location(_,0) => true
      case Location(_,_) => false

    def possible_actions(currentState: Location): List[Move] = currentState match
      case Location(0,y) => y match
        case 3 => List(Move.Right, Move.Down)
        case 0 => List(Move.Right, Move.Up)
        case _ => List(Move.Up, Move.Right, Move.Down)

      case Location(x,3) => x match
        case 11 => List(Move.Left,Move.Down)
        case 0  => List(Move.Right,Move.Down)
        case _  => List(Move.Right,Move.Left)

      case Location(_,_) => List(Move.Right,Move.Left,Move.Up,Move.Down)

    def is_out_of_bounds(move: Move, currentState: Location): Boolean =
      (currentState.x == 0 && move == Move.Left) || (currentState.x == 11 && move == Move.Right)
        || (currentState.y == 3 && move == Move.Up) || (currentState.y == 0 && move == Move.Down)

    def step(currentState:Location, action_taken: Move): (Location,Double) = currentState match
      case Location(11,0) => (currentState,0.0)
      case Location(x,y) if is_out_of_bounds(action_taken,currentState) => (currentState,-1.0)

      case Location(x,y)  =>
        val new_location = action_taken match
          case Move.Up    => Location(x,y+1)
          case Move.Down  => Location(x,y-1)
          case Move.Right => Location(x+1,y)
          case Move.Left  => Location(x-1,y)
        if isTerminal(new_location) && new_location != Location(11,0) then
          (currentState,-10)
        else
          (new_location,0.0)


  def main(args: Array[String]): Unit =
    val actions = List(Move.Up,Move.Down,Move.Left,Move.Right)
    val states : List[Location] = List.tabulate(12,4)((a,b) => Location(a,b)).foldRight(List.empty)(_:++_)
    val q_table = table_cons(states,actions)
    val rng = RNG.Simple(42)

    @tailrec
    def step(count: Int, environment: Environment[Location,Move], q_table: QTable[Location,Move], rng: RNG): QTable[Location,Move] =
      //println("stepping ")
      if count <= 0 then q_table
      else
        val new_q_table = episode(Location(0,0), environment, q_table, rng.nextInt._2, environment.step)
        step(count-1, environment, new_q_table, rng.nextInt._2.nextInt._2)

    val resulting_q_table = step(count = 400, environment, q_table, rng)
    println(resulting_q_table(Location(1,2)))

