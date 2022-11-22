import scala.io.Source

@main def binaryDiagnostic(input_path: String) = 
  // "1010" -> List(1, 0, 1, 0)
  val binStrToIntList = (x: String) => x.toCharArray.toList.map(x => Integer.parseInt(x.toString))
  val report = Source.fromFile(input_path).getLines().toList.map(binStrToIntList)
  
  def mostCommonBit(bits: List[Int]): Char =
    if (bits.sum > bits.size / 2) '1' else '0' 
  
  val gammaRateStr = report.transpose.map(mostCommonBit).mkString
  val gammaRate = BigInt(gammaRateStr, 2)
  val epsilonRate = BigInt(2).pow(gammaRateStr.length) - 1 - gammaRate
  println(gammaRate * epsilonRate)



