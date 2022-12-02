val values = io.Source.fromFile("input.txt").getLines.next().split(",").map(_.toInt).toList
def cost1(distance: Int) = distance
def cost2(distance: Int) = distance * (distance + 1) / 2
def minimalCost(values: List[Int], cost: Int => Int) =
  Range(values.min, values.max + 1).map(i => values.map(j => cost((i - j).abs)).sum).min

println(minimalCost(values, cost1))
println(minimalCost(values, cost2))
