import scala.collection.mutable as mutable

@main
def solve_problem12: Unit = {
  val g = Problem12.parse(Problem12.input)
  val paths = Problem12.travel(g, Seq(), Problem12.Node.Small("start"))
  println(paths.size)

}

object Problem12 {
  enum Node {
    case Big(id: String)
    case Small(id: String)

    def isSmall: Boolean = this match {
      case Small(_) => true
      case _        => false
    }

    def getId: String = this match {
      case Big(id)   => id
      case Small(id) => id
    }

    def isStart: Boolean = this match {
      case Small("start") => true
      case _              => false
    }

    def isEnd: Boolean = this match {
      case Small("end") => true
      case _            => false
    }
    override def toString: String = this match {
      case Big(id)   => s"($id)"
      case Small(id) => s"[$id]"
    }

  }
  object Node {
    def fromString(s: String): Node = {
      if (s.forall(_.isLower)) {
        Small(s)
      } else {
        Big(s)
      }
    }
  }

  type Graph = Map[Node, Set[Node]]

  def valid_path2(path: Seq[Node]): Boolean = {
    val smalls = path.filter(_.isSmall).groupBy(_.getId).mapValues(_.size)
    val starts = path.filter(_.isStart)
    val smalls_sum = smalls.values.sum
    val smalls_size = smalls.size + 1
    starts.size == 1 && smalls_sum <= smalls_size
  }

  def travel(g: Graph, path: Seq[Node], candidate: Node): Set[Seq[Node]] = {
    val new_path = path.appended(candidate)
    if !valid_path2(new_path) then Set()
    else if candidate.isEnd then Set(new_path)
    else
      val next = for n <- g(candidate) yield travel(g, new_path, n)
      next.flatten
  }

  def parse(s: String): Graph = {
    val lines = s.split("\n")
    val edges = lines.map(line => {
      val parts = line.split("-")
      val nodeFrom = Node.fromString(parts(0))
      val nodeTo = Node.fromString(parts(1))
      Seq(nodeFrom -> nodeTo, nodeTo -> nodeFrom)
    })
    edges.flatten
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
      .toMap
      .withDefaultValue(Set())
  }

  val input = """rf-RL
rf-wz
wz-RL
AV-mh
end-wz
end-dm
wz-gy
wz-dm
cg-AV
rf-AV
rf-gy
end-mh
cg-gy
cg-RL
gy-RL
VI-gy
AV-gy
dm-rf
start-cg
start-RL
rf-mh
AV-start
qk-mh
wz-mh"""

  val sample_input3 = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""

  val sample_input2 = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""

  val sample_input = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
}
