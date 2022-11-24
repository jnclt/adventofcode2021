import scala.io.Source

import collection.mutable.Set

@main def giantSquid(inputPath: String): Unit =
  case class Board(rows: List[Set[Byte]], columns: List[Set[Byte]])

  def parse(lines: Iterator[String]): (List[Board], List[Byte]) =
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

    val boards = toSet(rows).zip(toSet(columns)).map(pair => Board(pair._1, pair._2))
    (boards, draws)

  val (boards, draws) = parse(Source.fromFile(inputPath).getLines())

  def round(draw: Byte): Unit =
    def strikeBoard(board: Board): Unit =
      def strikeBoardDirection(boardDirection: List[Set[Byte]]): Unit =
        boardDirection.foreach(set => {
          set.remove(draw)
          if set.isEmpty then
            println(boardDirection.flatten.map(_.toInt).sum * draw)
            sys.exit
        })
      strikeBoardDirection(board.rows)
      strikeBoardDirection(board.columns)
    boards.map(strikeBoard)
  draws.foreach(round)
