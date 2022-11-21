import scala.io.Source

@main def dove(input_path: String) = {

  enum Direction:
    case Forward, Down, Up

  case class Move(direction: Direction, value: Int)

  def to_move(str: String): Move = {
    str.split(" ").toList match {
      case move :: value :: Nil =>
        move match {
          case "forward" => Move(Direction.Forward, value.toInt)
          case "up"      => Move(Direction.Up, value.toInt)
          case "down"    => Move(Direction.Down, value.toInt)
        }
    }
  }

  val moves =
    Source.fromFile(input_path).getLines().toList.map(to_move)
  println(destination_product(moves))
  println(aimed_destination_product(moves))

  def destination_product(moves: List[Move]): Int = {
    val move_groups = moves.groupBy(_.direction)

    def distance(key: Direction): Int = {
      move_groups.apply(key).map(_.value).sum(Numeric[Int])
    }
    distance(Direction.Forward) * (distance(Direction.Down) - distance(
      Direction.Up
    ))
  }

  def aimed_destination_product(moves: List[Move]): Int = {
    case class State(hdistance: Int, depth: Int, aim: Int)
    def execute(acc: State, move: Move): State = {
      move.direction match {
        case Direction.Forward =>
          acc.copy(
            hdistance = acc.hdistance + move.value,
            depth = acc.depth + (move.value * acc.aim)
          )
        case Direction.Up =>
          acc.copy(
            aim = acc.aim - move.value
          )
        case Direction.Down =>
          acc.copy(
            aim = acc.aim + move.value
          )
      }
    }

    val state = moves.foldLeft(State(0, 0, 0))(execute)
    state.hdistance * state.depth
  }
}
