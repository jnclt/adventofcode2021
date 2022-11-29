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
    def steps(coord: Point => Int): List[Int] =
      val step = if coord(start) <= coord(end) then 1 else -1
      List.range(coord(start), coord(end) + step, step)
    def points: List[Point] =
      if horizontal then steps(_.x).map(x => Point(x, start.y))
      else if vertical then steps(_.y).map(y => Point(start.x, y))
      else steps(_.x).zip(steps(_.y)).map(p => Point(p._1, p._2))

  def parse(lines: Iterator[String]): List[Line] =
    val coords = """^(\d+),(\d+) -> (\d+),(\d+)$""".r
    lines
      .map(l =>
        l match
          case coords(sx, sy, ex, ey) =>
            Line(Point(sx.toInt, sy.toInt), Point(ex.toInt, ey.toInt))
      )
      .toList

  def hotspotCount(lines: List[Line]): Int =
    lines
      .flatMap(_.points)
      .groupBy[Point](identity)
      .values
      .map(_.size)
      .filter(_.>=(2))
      .size

  val lines = parse(Source.fromFile(inputPath).getLines())
  println(hotspotCount(lines.filter(_.orthogonal)))
  println(hotspotCount(lines))

hydrothermalVenture(args(0))
