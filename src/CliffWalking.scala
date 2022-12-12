package adpro.rl

import adpro.rl.QLearning.*
import adpro.rl.QLearning.QTable
import adpro.rl.RNG

import scala.annotation.tailrec

object CliffWalking:

  case class Location(x: Int ,y: Int)

  enum Move:
    case Up,Down,Right,Left

  implicit val environment : Environment[Location,Move] = new Environment[Location,Move]:
    def isTerminal(state:Location): Boolean = state match
      case Location(0,0) => false
      case Location(_, 0) => true
      case Location(_, _) => false

    def isOutOfBounds(move: Move, currentState: Location): Boolean =
      (currentState.x == 0 && move == Move.Left) || (currentState.x == 11 && move == Move.Right)
        || (currentState.y == 3 && move == Move.Up) || (currentState.y == 0 && move == Move.Down)

    def step(currentState:Location, actionTaken: Move): (Location,Double) = currentState match
      case Location(11, 0) => (currentState,0.0)
      case Location(x, y) if isOutOfBounds(actionTaken, currentState) => (currentState, -1.0)

      case Location(x,y)  =>
        val newLocation = actionTaken match
          case Move.Up    => Location(x, y+1)
          case Move.Down  => Location(x, y-1)
          case Move.Right => Location(x+1, y)
          case Move.Left  => Location(x-1, y)
        if isTerminal(newLocation) && newLocation != Location(11,0) then
          (currentState, -10)
        else
          (newLocation, 0.0)

  def traverse(qtable: QTable[Location, Move], currentLocation: Location): Unit =
    print(currentLocation)
    //println(qtable)
    if currentLocation != Location(11, 0) then
      val newState = qtable(currentLocation).toList.maxBy(_._2)._1 match
        case Move.Up    => Location(currentLocation.x, currentLocation.y + 1)
        case Move.Down  => Location(currentLocation.x, currentLocation.y - 1)
        case Move.Left  => Location(currentLocation.x - 1, currentLocation.y)
        case Move.Right => Location(currentLocation.x + 1, currentLocation.y)
      traverse(qtable, newState)

  def main(args: Array[String]): Unit =
    val actions = List(Move.Up,Move.Down,Move.Left,Move.Right)
    val states : List[Location] = List.tabulate(12,4)((a,b) => Location(a,b)).foldRight(List.empty)(_:++_)
    val qtable = QTable[Location, Move](states,actions)
    val rng = RNG.Simple(42)

    @tailrec
    def step(count: Int, environment: Environment[Location,Move], qtable: QTable[Location,Move], rng: RNG): QTable[Location,Move] =
      if count <= 0 then qtable
      else
        val newQTable = episode(Location(0,0), environment, qtable, rng.nextInt._2, environment.step)
        step(count-1, environment, newQTable, rng.nextInt._2.nextInt._2)

    val finalQTable = step(4000, environment, qtable, rng)
    println(finalQTable)
    println(traverse(finalQTable, Location(0,0)))
