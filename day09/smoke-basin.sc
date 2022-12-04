val grid = io.Source.fromFile("input.txt").getLines.map(_.toCharArray.toVector).toVector
val Max = ('9'.toInt + 1).toChar
def neighbor(x: Int, y: Int): Char = grid.applyOrElse(x, _ => Vector.empty).applyOrElse(y, _ => Max)
def neighbors(x: Int, y: Int) =
  List(neighbor(x - 1, y), neighbor(x + 1, y), neighbor(x, y - 1), neighbor(x, y + 1))
val lowPoints = Range(0, grid.size).flatMap(x => Range(0, grid(x).size).map(y => (x, y))).filter {
  case (x, y) => neighbors(x, y).forall(_ > grid(x)(y))
}
println(lowPoints.map((x, y) => grid(x)(y).asDigit).sum + lowPoints.size)
