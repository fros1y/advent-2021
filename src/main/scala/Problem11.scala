import scala.collection.mutable as mutable
import Problem11.Octopus
type Cave = mutable.IndexedBuffer[mutable.IndexedBuffer[Octopus]]

@main
def solve_problem11: Unit = {
  var cave = Problem11.parsed
  println(cave.map(_.mkString("")).mkString("\n"))
  println()

  val flashes = Stream
    .from(0)
    .map(i => {
      Problem11.step(i, cave)
      (i, cave.flatten.filter(_.flashed).size)
    })

  val (i, flashed) = flashes.dropWhile(_._2 < cave.flatten.size).head

  println(s"Sync at ${i + 1}")
}

object Problem11 {

  case class Octopus(var energy: BigInt, var flashed: Boolean) {

    def can_flash(step_number: BigInt): Boolean =
      !flashed && energy > 9
    def flash(step_number: BigInt): Unit = {
      energy = 0; flashed = true
    }
    def add_energy(delta: BigInt): Unit = { if (!flashed) { energy += delta } }
    override def toString: String = s"$energy"
  }

  object Octopus {
    def apply(energy: Int): Octopus = Octopus(energy, false)
  }

  def neighbors(max_x: Int, max_y: Int, x: Int, y: Int): Seq[(Int, Int)] =
    for
      dx <- -1 to 1
      dy <- -1 to 1
      if dx != 0 || dy != 0
      new_x = x + dx
      new_y = y + dy
      if new_x >= 0
      if new_y >= 0
      if new_x < max_x
      if new_y < max_y
    yield (new_x, new_y)

  def cells(cave: Cave): Seq[(Int, Int)] = {
    val max_x = cave.length
    val max_y = cave(0).length
    for
      x <- 0 to max_x - 1
      y <- 0 to max_y - 1
    yield (x, y)
  }

  def step(i: Int, cave: Cave): Unit = {

    cells(cave).foreach((x, y) => {
      cave(x)(y).flashed = false
      cave(x)(y).add_energy(1)
    })

    while (cells(cave).exists((x, y) => cave(x)(y).can_flash(i))) {
      cells(cave)
        .filter((x, y) => cave(x)(y).can_flash(i))
        .foreach((x, y) => {
          cave(x)(y).flash(i)
          neighbors(cave.length, cave(0).length, x, y)
            .foreach((dx, dy) => cave(dx)(dy).add_energy(1))
        })
    }

  }
  def parsed = input
    .split("\n")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.split("").map(_.toInt).map(Octopus.apply).to(mutable.IndexedBuffer))
    .to(mutable.IndexedBuffer)

  val input = """2682551651
3223134263
5848471412
7438334862
8731321573
6415233574
5564726843
6683456445
8582346112
4617588236"""

  val sample_input = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""
}
