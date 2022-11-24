import scala.io.Source

import collection.mutable.Set

@main def giantSquid(inputPath: String): Unit =
  case class Board(rows: List[Set[Byte]], columns: List[Set[Byte]])

  def parse(lines: Iterator[String]): (Set[Board], List[Byte]) =
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

    val boards = Set(toSet(rows).zip(toSet(columns)).map(pair => Board(pair._1, pair._2))*)
    (boards, draws)

  var (boards, draws) = parse(Source.fromFile(inputPath).getLines())

  def round(draw: Byte): List[Int] =
    def isWinning(board: Board): Boolean =
      def isWinningDirection(boardDirection: List[Set[Byte]]): Boolean =
        boardDirection
          .map(set => {
            set.remove(draw)
            set.isEmpty
          })
          .reduce(_ || _)
      isWinningDirection(board.rows) || isWinningDirection(board.columns)
    val (won, active) = boards.partition(isWinning)
    boards = active
    won.map(board => board.rows.flatten.map(_.toInt).sum * draw).toList

  val results = draws.flatMap(round)
  println(results.head) // part1: winning board score
  println(results.last) // part2: last board score
