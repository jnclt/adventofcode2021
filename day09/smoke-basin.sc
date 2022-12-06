val grid = io.Source.fromFile("input.txt").getLines.map(_.toCharArray.toVector).toVector
def neighborCoords(x: Int, y: Int) =
  if x < 0 || y < 0 || x >= grid.size || y >= grid(x).size then None
  else Some((x, y))
def neighborsCoords(x: Int, y: Int) =
  List(
    neighborCoords(x - 1, y),
    neighborCoords(x + 1, y),
    neighborCoords(x, y - 1),
    neighborCoords(x, y + 1)
  ).flatten.toSet
def neighbors(x: Int, y: Int) =
  neighborsCoords(x, y).map { case (a, b) => grid(a)(b) }

val lowPoints = Range(0, grid.size).flatMap(x => Range(0, grid(x).size).map(y => (x, y))).filter {
  case (x, y) => neighbors(x, y).forall(_ > grid(x)(y))
}
println(lowPoints.map((x, y) => grid(x)(y).asDigit).sum + lowPoints.size)

def floodFill(x: Int, y: Int, flooded: Set[(Int, Int)]): Set[(Int, Int)] =
  val newFlooded =
    neighborsCoords(x, y).filterNot(flooded).filter { case (x, y) => grid(x)(y) < '9' }
  newFlooded.foldLeft(flooded ++ newFlooded) { case (flooded, (x, y)) => floodFill(x, y, flooded) }
val basins = lowPoints.map { case (x, y) => floodFill(x, y, Set((x, y))).size }
println(basins.sorted.takeRight(3).product)
