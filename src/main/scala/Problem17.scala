@main
def solve_problem17: Unit = {
  val input = Problem17.parse(Problem17.input)

  val trajectories = (for
    f <- 0 to input.x_range.end
    v <- -(input.y_range.min.abs) to input.y_range.min.abs
    trajectory = Problem17.Trajectory(f, v)
    //_ = println(s"$f, $v")
    path = input.launch(trajectory)
    if input.landed_on_target(path)
  yield trajectory).toSet

  //val max_y = paths.flatten.map(_._2).max
  //println(max_y)
  println(trajectories.size)
}

object Problem17 {
  type Coord = (Int, Int)

  case class Trajectory(forward: Int, vertical: Int) {
    def launch: Iterator[Coord] = {
      Iterator
        .iterate(((0, 0), this)) { case ((x, y), t) =>
          ((x + t.forward, y + t.vertical), t.update)
        }
        .map(_._1)
    }
    def update: Trajectory = forward match {
      case _ if forward == 0 => Trajectory(forward, vertical - 1)
      case _ if forward > 0  => Trajectory(forward - 1, vertical - 1)
      case _ if forward < 0  => Trajectory(forward + 1, vertical - 1)
    }
  }

  case class Target(x_range: Range, y_range: Range) {
    def is_in_range(coord: Coord): Boolean = {
      x_range.contains(coord._1) && y_range.contains(coord._2)
    }
    def not_past_target(coord: Coord): Boolean = {
//      println(s"$coord")
      //    println(s"${x_range.end} ${y_range.end}")
      coord._1 <= x_range.end && coord._2 >= y_range.start
    }

    def launch(t: Trajectory): Seq[Coord] = {
      t.launch.takeWhile(c => not_past_target(c)).toSeq
    }
    def landed_on_target(path: Seq[Coord]): Boolean = path.exists(is_in_range)
  }

  def parse(s: String): Target = {
    val pattern = """(-?\d+)\.\.(-?\d+)""".r
    val matched = pattern.findAllIn(s).matchData.toList
    val x_start = matched(0).group(1).toInt
    val x_end = matched(0).group(2).toInt
    val y_start = matched(1).group(1).toInt
    val y_end = matched(1).group(2).toInt
    return Target(
      Range(x_start, x_end).inclusive,
      Range(y_start, y_end).inclusive
    )
  }

  val sample_input = """target area: x=20..30, y=-10..-5"""
  val input = """target area: x=102..157, y=-146..-90"""
}
