import scala.io.Source

@main def binaryDiagnostic(input_path: String) =
  // "1010" -> List(1, 0, 1, 0)
  val binStrToIntList = (x: String) => x.toCharArray.toList.map(x => Integer.parseInt(x.toString))
  val report = Source.fromFile(input_path).getLines().toList.map(binStrToIntList)

  def mostCommonBit(bits: List[Int]): Int =
    if (bits.sum >= bits.size / 2.0) 1 else 0

  val gammaRateStr = report.transpose.map(mostCommonBit).mkString
  val gammaRate = BigInt(gammaRateStr, 2)
  val epsilonRate = BigInt(2).pow(gammaRateStr.length) - 1 - gammaRate
  println(gammaRate * epsilonRate)

  def leastCommonBit(bits: List[Int]): Int =
    1 - mostCommonBit(bits)

  def filterReport(criterion: (List[Int]) => Int, set: List[List[Int]]): List[Int] =
    set match
      case x :: Nil =>
        x
      case _ =>
        val bitValue = criterion(set.map(_.head))
        bitValue :: filterReport(criterion, set.filter(bits => bits.head == bitValue).map(_.tail))

  val o2Generator = Integer.parseInt(filterReport(mostCommonBit, report).mkString, 2)
  val co2Scrubber = Integer.parseInt(filterReport(leastCommonBit, report).mkString, 2)
  println(o2Generator * co2Scrubber)
