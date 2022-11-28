import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def hydrothermalVenture(inputPath: String): Unit =
  case class Point(x: Int, y: Int)
  case class Line(start: Point, end: Point):
    def horizontal: Boolean =
      start.y == end.y
    def vertical: Boolean =
      start.x == end.x
    def orthogonal: Boolean =
      horizontal || vertical
    def left: Int =
      List(start.x, end.x).min
    def right: Int =
      List(start.x, end.x).max
    def top: Int =
      List(start.y, end.y).min
    def bottom: Int =
      List(start.y, end.y).max
    def points: List[Point] =
      if horizontal then List.range(left, right + 1).map(x => Point(x, top))
      else List.range(top, bottom + 1).map(y => Point(left, y))

  def parse(lines: Iterator[String]): List[Line] =
    val coords = """^(\d+),(\d+) -> (\d+),(\d+)$""".r
    lines
      .map(l =>
        l match
          case coords(sx, sy, ex, ey) =>
            Line(Point(sx.toInt, sy.toInt), Point(ex.toInt, ey.toInt))
      )
      .toList
  val lines = parse(Source.fromFile(inputPath).getLines())

  val hotspotCount = lines
    .filter(_.orthogonal)
    .flatMap(_.points)
    .groupBy[Point](identity)
    .values
    .map(_.size)
    .filter(_.>=(2))
    .size
  println(hotspotCount)

hydrothermalVenture(args(0))
