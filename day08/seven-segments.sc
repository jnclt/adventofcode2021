val lines = io.Source.fromFile("input.txt").getLines.toList
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
