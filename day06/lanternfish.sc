import scala.io.Source

def lanternfish(inputPath: String): Unit =
  val initial_ages = Source.fromFile(inputPath).getLines.next.split(",").toList.map(_.toInt)
  val countByAge = initial_ages.groupMapReduce(identity)(_ => 1)(_ + _)
  val sizes: scala.collection.mutable.Map[Int, BigInt] = scala.collection.mutable.Map()

  def childrenCount(remainingDays: Int): Int =
    if remainingDays < 0 then 0 else remainingDays / 7 + 1

  def familySize(remainingDays: Int): BigInt =
    sizes.getOrElseUpdate(
      remainingDays, {
        1 + Range(0, childrenCount(remainingDays))
          .map(childNo => familySize(remainingDays - childNo * 7 - 9))
          .sum
      }
    )

  def populationSize(days: Int): BigInt =
    countByAge.map((age, count) => count * familySize(days - 1 - age)).sum

  println(populationSize(80))
  println(populationSize(256))

lanternfish(args(0))
