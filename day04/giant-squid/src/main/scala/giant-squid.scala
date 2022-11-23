import scala.io.Source

import collection.mutable.Set

@main def giantSquid(inputPath: String): Unit =
  case class Boards(rows: List[List[Set[Byte]]], columns: List[List[Set[Byte]]])

  def parse(lines: Iterator[String]): (Boards, List[Byte]) =
    val draws = lines.next().split(",").map(_.toByte).toList
    lines.next()

    val rows =
      lines
        .filter(!_.isBlank)
        .map(line => line.trim.split("\\s+").map(_.toByte).toList)
        .toList
        .grouped(5)
        .toList
    val columns = rows.map(_.transpose)

    def toSet(boards: List[List[List[Byte]]]): List[List[Set[Byte]]] =
      boards.map(_.map(set => Set(set*)))
    (Boards(toSet(rows), toSet(columns)), draws)

  val (boards, draws) = parse(Source.fromFile(inputPath).getLines())

  def round(draw: Byte): Unit =
    def strike(draw: Byte, sets: List[List[Set[Byte]]]): Unit =
      sets.foreach(board =>
        board.foreach(set => {
          set.remove(draw)
          if set.isEmpty then
            println(board.flatten.map(_.toInt).sum * draw)
            sys.exit
        })
      )

    strike(draw, boards.rows)
    strike(draw, boards.columns)

  draws.foreach(round)
