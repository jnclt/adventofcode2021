val values = io.Source.fromFile("input.txt").getLines.next().split(",").map(_.toInt).toList
println(
  Range(values.min, values.max + 1)
    .map(i => values.map(j => (i - j).abs).sum)
    .min
)
