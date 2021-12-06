import scala.collection.mutable.HashMap
import scala.collection.immutable.IntMap
import scala.annotation.tailrec

@main
def solve_problem6(): Unit = {
  val initial_population = IntMap.from(
    Problem6.input
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .mapValues(x => BigInt(x.size))
      .toIterable
  )
  println(initial_population.mkString(","))
  for (fish, r) <- Problem6.run(initial_population).take(256).zipWithIndex do {
    val total: BigInt = fish.values.sum
    println(s"$r: $total")
  }
}

object Problem6 {
  def transition(fish: IntMap[BigInt]): IntMap[BigInt] = merge(fish.map {
    case (k, v) =>
      if k > 0 then IntMap((k - 1, v)) else IntMap((6, v), (8, v))
  }.toSeq)

  def merge(maps: Seq[IntMap[BigInt]]): IntMap[BigInt] = {
    maps.foldLeft(IntMap.empty[BigInt]) { case (acc, m) =>
      acc.unionWith(m, { case (_, a, b) => a + b })
    }
  }

  def run(fish: IntMap[BigInt]): Stream[IntMap[BigInt]] = {
    val next = transition(fish)
    next #:: run(next)
  }

  def sample_input = """3,4,3,1,2"""
  def input =
    """3,5,1,2,5,4,1,5,1,2,5,5,1,3,1,5,1,3,2,1,5,1,1,1,2,3,1,3,1,2,1,1,5,1,5,4,5,5,3,3,1,5,1,1,5,5,1,3,5,5,3,2,2,4,1,5,3,4,2,5,4,1,2,2,5,1,1,2,4,4,1,3,1,3,1,1,2,2,1,1,5,1,1,4,4,5,5,1,2,1,4,1,1,4,4,3,4,2,2,3,3,2,1,3,3,2,1,1,1,2,1,4,2,2,1,5,5,3,4,5,5,2,5,2,2,5,3,3,1,2,4,2,1,5,1,1,2,3,5,5,1,1,5,5,1,4,5,3,5,2,3,2,4,3,1,4,2,5,1,3,2,1,1,3,4,2,1,1,1,1,2,1,4,3,1,3,1,2,4,1,2,4,3,2,3,5,5,3,3,1,2,3,4,5,2,4,5,1,1,1,4,5,3,5,3,5,1,1,5,1,5,3,1,2,3,4,1,1,4,1,2,4,1,5,4,1,5,4,2,1,5,2,1,3,5,5,4,5,5,1,1,4,1,2,3,5,3,3,1,1,1,4,3,1,1,4,1,5,3,5,1,4,2,5,1,1,4,4,4,2,5,1,2,5,2,1,3,1,5,1,2,1,1,5,2,4,2,1,3,5,5,4,1,1,1,5,5,2,1,1"""
}
