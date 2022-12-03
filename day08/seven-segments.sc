val lines = io.Source.fromFile("input.txt").getLines.toList
// part1
println(
  lines
    .flatMap(
      _.split(""" \| """).last
        .split(""" """)
        .map(_.size)
        .filter(s => s < 5 || s > 6)
    )
    .size
)

def decode(signals: Set[Set[Char]]): Map[Set[Char], Char] =
  val one = signals.find(_.size == 2).get
  val four = signals.find(_.size == 4).get
  val seven = signals.find(_.size == 3).get
  val eight = signals.find(_.size == 7).get

  val zeroSixNine = signals.filter(_.size == 6)
  val six = zeroSixNine.find(s => (s & one).size == 1).get
  val zero = zeroSixNine.-(six).find(s => (s & four).size == 3).get
  val nine = zeroSixNine.-(six).-(zero).head

  val twoThreeFive = signals.filter(_.size == 5)
  val two = twoThreeFive.find(s => (s & four).size == 2).get
  val three = twoThreeFive.-(two).find(s => (s & seven).size == 3).get
  val five = twoThreeFive.-(two).-(three).head

  Map(
    zero -> '0',
    one -> '1',
    two -> '2',
    three -> '3',
    four -> '4',
    five -> '5',
    six -> '6',
    seven -> '7',
    eight -> '8',
    nine -> '9'
  )

def evaluate(digits: List[Set[Char]], signals: Set[Set[Char]]): Int =
  val toDigits = decode(signals)
  digits.map(toDigits).mkString.toInt

// part2
println(
  lines
    .map(l =>
      val parts = l.split(""" \| """)
      evaluate(
        parts(1).split(""" """).map(_.toSet).toList,
        parts(0).split(""" """).map(_.toSet).toSet
      )
    )
    .sum
)
