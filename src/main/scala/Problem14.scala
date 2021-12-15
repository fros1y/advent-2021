import scala.collection.mutable as mutable
import scala.collection.parallel.CollectionConverters._

@main
def solve_problem14: Unit = {
  val (input, rules_p) = Problem14.parse(Problem14.input)
  implicit val rules = rules_p
  val output =
    Problem14.ntimes(40, Problem14.rewrite_b, input.toList)
  val freqs = output.groupBy(identity).mapValues(x => BigInt(x.size))
  val minFreq = freqs.values.min
  val maxFreq = freqs.values.max
  println("output: " + (maxFreq - minFreq))

}
def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}
object Problem14 {
  type Input = List[Char]
  type Output = Char
  type Rule = (Input, Output)
  type Rules = Map[Input, Output]

  def ntimes[A](n: Int, f: A => A, a: A): A =
    if (n == 0) a
    else {
      println(s"$n")
      ntimes(n - 1, f, f(a))
    }

  def rewrite_c(input: Input)(using rules: Rules): List[Char] = input match {
    case Nil => Nil
    case a :: b :: Nil if rules.contains(List(a, b)) =>
      List(a, rules(List(a, b)), b)
    case _ => {
      val (a, b) = input.splitAt(input.size / 2)
      val a_ = rewrite_c(a)
      val b_ = rewrite_c(b)
      a_ ++ rewrite_c(List(a_.last, b_.head)) ++ b_
    }
  }

  def rewrite_b(input: Input)(using rules: Rules): List[Char] =
    (for i <- input.indices.par yield {
      val slice = input.slice(i, i + 2)
      val rule = rules.get(slice)
      rule match {
        case Some(c) => Seq(input(i), c)
        case None    => Seq(input(i))
      }
    }).flatten.toList

  def rewrite(input: Input)(using rules: Rules): List[Char] = input match {
    case x if x.length < 2 => x
    case a :: rest if rules.contains(List(a, rest.head)) =>
      a :: rules(List(a, rest.head)) :: rewrite(rest)
    case a :: rest => a :: rewrite(rest)
  }

  val input = """NCOPHKVONVPNSKSHBNPF

ON -> C
CK -> H
HC -> B
NP -> S
NH -> H
CB -> C
BB -> H
BC -> H
NN -> C
OH -> B
SF -> V
PB -> H
CP -> P
BN -> O
NB -> B
KB -> P
PV -> F
SH -> V
KP -> S
OF -> K
BS -> V
PF -> O
BK -> S
FB -> B
SV -> B
BH -> V
VK -> N
CS -> V
FV -> F
HS -> C
KK -> O
SP -> N
FK -> B
CF -> C
HP -> F
BF -> O
KC -> C
VP -> O
BP -> P
FF -> V
NO -> C
HK -> C
HV -> B
PK -> P
OV -> F
VN -> H
PC -> K
SB -> H
VO -> V
BV -> K
NC -> H
OB -> S
SN -> B
HF -> P
VF -> B
HN -> H
KS -> S
SC -> S
CV -> B
NS -> P
KO -> V
FS -> O
PH -> K
BO -> C
FH -> B
CO -> O
FO -> F
VV -> N
CH -> V
NK -> N
PO -> K
OK -> K
PP -> O
OC -> P
FC -> N
VH -> S
PN -> C
VB -> C
VS -> P
HO -> F
OP -> S
HB -> N
CC -> K
KN -> S
SK -> C
OS -> N
KH -> B
FP -> S
NF -> S
CN -> S
KF -> C
SS -> C
SO -> S
NV -> O
FN -> B
PS -> S
HH -> C
VC -> S
OO -> C
KV -> P"""

  val sample_input = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

  def parse(input: String): (String, Map[Input, Output]) = {
    val lines = input.split("\n")
    val start = lines.head.trim
    val rules = lines.tail
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { line =>
        val parts = line.split(" -> ")
        val from = parts.head
        val to = parts.last
        (
          from.toList,
          to.toCharArray.head
        )
      }
      .toMap
    (start, rules)
  }
}
