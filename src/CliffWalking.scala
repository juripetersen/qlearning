package src
import src.Main.*
import src.State.RNG


object CliffWalking:

  case class Location(x: Int ,y: Int)

  enum Move:
    case Up,Down,Right,Left

  implicit val environment : Environment[Location,Move] = new Environment[Location,Move]:
    def isTerminal(state:Location): Boolean = state match
      case Location(0,0) => false
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

    def step(currentState:Location, action_taken: Move): (Location,Double) = currentState match
      case Location(12,0) => (currentState,0.0)
      case Location(x,y)  =>
        val new_location = action_taken match
          case Move.Up    => Location(x,y+1)
          case Move.Down  => Location(x,y-1)
          case Move.Right => Location(x+1,y)
          case Move.Left  => Location(x-1,y)
        (new_location,0.0)

  def main(args: Array[String]): Unit =
    val actions = List(Move.Up,Move.Down,Move.Left,Move.Right)
    val states : List[Location] = List.tabulate(4,12)((a,b) => Location(a,b)).foldRight(List.empty)(_:++_)
    val q_table = table_cons(states,actions)
    val rng = RNG.Simple(42)

    episode(Location(0,0),environment,q_table, rng, environment.step)

