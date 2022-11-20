import scala.io.Source

object Sonar extends App {
  val measurements = Source.fromFile(args(0)).getLines().toList.map(_.toInt)
  println(get_count_of_depth_increases(measurements.sliding(1).toList, 0))
  println(get_count_of_depth_increases(measurements.sliding(3).toList, 0))

  def get_count_of_depth_increases(windowed_measurements: List[List[Int]], acc: Int): Int = {
    windowed_measurements match {
      case head :: next :: tail => get_count_of_depth_increases(next :: tail, acc + {if (head.sum < next.sum) 1 else 0})
      case _ => acc
    }
  }
}