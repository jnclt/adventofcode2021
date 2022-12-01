import scala.io.Source

def lanternfish(inputPath: String): Unit =
  val initial_ages = Source.fromFile(inputPath).getLines.next.split(",").toList.map(_.toInt)
  val countByAge = initial_ages.groupMapReduce(identity)(_ => 1)(_ + _)
  val days = 80

  def childrenCount(remainingDays: Int): Int =
    if remainingDays < 0 then 0 else remainingDays / 7 + 1

  def familySize(remainingDays: Int): BigInt =
    val size = 1 + Range(0, childrenCount(remainingDays))
      .map(childNo => familySize(remainingDays - childNo * 7 - 9))
      .sum
    size

  val total = countByAge.map((age, count) => count * familySize(days - 1 - age)).sum
  println(total)

lanternfish(args(0))
