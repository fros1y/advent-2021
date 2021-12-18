import parsley.Parsley, Parsley._
import parsley.errors._
import parsley.lift._
import parsley.character.{
  char,
  string,
  digit,
  anyChar,
  letter,
  whitespace,
  newline,
  space,
  satisfy
}
import parsley.implicits.character.{charLift, stringLift}
import parsley.combinator._
import parsley.{Result, Success, Failure}

import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
import parsley.character.isWhitespace

@main
def solve_problem18 = {
  import Problem18._
  val parsed = parse(input)
  val sum = parsed.tail.foldLeft(parsed.head)((a, b) => a + b)
  println(sum.magnitude)

  val magnitudes = for
    a <- parsed
    b <- parsed
    if a != b
  yield (a + b).magnitude
  println(magnitudes.max)
}
object Problem18 {

  def whileSome[A](initial: A, f: A => Option[A]): Stream[A] = {
    def loop(a: A): Stream[A] = f(a) match {
      case Some(a) => a #:: loop(a)
      case None    => Stream.empty
    }
    initial #:: loop(initial)
  }

  enum Snailadic {
    case Number(x: Int)
    case Pair(fst: Snailadic, snd: Snailadic)

    def magnitude: Int = this match {
      case Number(x)      => x
      case Pair(fst, snd) => fst.magnitude * 3 + snd.magnitude * 2
    }

    override def toString = this match {
      case Number(x)      => x.toString
      case Pair(fst, snd) => s"[$fst,$snd]"
    }
    def +(that: Snailadic): Snailadic = Pair(this, that).reduce

    def getNumber: Option[Int] = this match {
      case Number(x) => Some(x)
      case _         => None
    }
    def isPair: Boolean = this match {
      case Pair(_, _) => true
      case _          => false
    }

    def reduce: Snailadic =
      whileSome(this, x => x.step).lastOption.getOrElse(this)

    def step: Option[Snailadic] = {
      val exploded = whileSome(this, x => x.explode_step).lastOption
      val split = exploded.getOrElse(this).split_step

      (exploded, split) match {
        case (_, Some(y)) if y != this    => Some(y)
        case (Some(x), None) if x != this => Some(x)
        case _                            => None
      }
    }

    def split_step: Option[Snailadic] = for
      target <- Loc(this, Context.Top, 0).firstSplitNumber
      value <- target.tree.getNumber
    yield target
      .update(
        Pair(
          Number((value / 2.0).floor.toInt),
          Number((value / 2.0).ceil.toInt)
        )
      )
      .upmost
      .tree

    def explode_step: Option[Snailadic] = {
      for {
        target <- Loc(this, Context.Top, 0).firstExplodePair
        left <- target.left
        left_value <- left.tree.getNumber
        right <- target.right
        right_value <- right.tree.getNumber
        new_target = target.update(Number(0))
        lefted_updated = (for {
          update_location <- new_target.prev
          orig_value <- update_location.tree.getNumber
          updated = update_location.update(Number(left_value + orig_value))
          restored <- updated.next
        } yield restored).getOrElse(new_target)

        righted_updated = (for {
          update_location <- lefted_updated.next
          orig_value <- update_location.tree.getNumber
          updated = update_location.update(Number(right_value + orig_value))
          restored <- updated.prev
        } yield restored).getOrElse(lefted_updated)

      } yield righted_updated.upmost.tree
    }

  }
  import Snailadic._

  enum Context {
    case Top
    case L(ctx: Context, tree: Snailadic)
    case R(tree: Snailadic, ctx: Context)
  }
  import Context._

  case class Loc(tree: Snailadic, context: Context, depth: Int = 0) {
    def firstSplitNumber: Option[Loc] = tree match {
      case Number(x) if x >= 10 => Some(this)
      case Pair(l, r) =>
        val lep = this.left.get.firstSplitNumber
        if (lep.isDefined) then lep else this.right.get.firstSplitNumber
      case _ => None
    }

    def firstExplodePair: Option[Loc] =
      tree match {
        case Number(_)                                    => None
        case Pair(Number(_), Number(_)) if this.depth > 3 => Some(this)
        case Pair(Number(_), Number(_))                   => None
        case Pair(l, r) => {
          val lep = this.left.get.firstExplodePair
          if (lep.isDefined) then lep else this.right.get.firstExplodePair
        }
      }

    def first: Option[Loc] = {
      (context, tree) match {
        case (_, Snailadic.Number(x))  => Some(this)
        case (c, Snailadic.Pair(l, r)) => this.left.get.first
      }
    }
    def last: Option[Loc] = {
      (context, tree) match {
        case (_, Snailadic.Number(x))  => Some(this)
        case (c, Snailadic.Pair(l, r)) => this.right.get.last
      }
    }

    def prev: Option[Loc] = for {
      upped <- this.up
      lefted <- upped.left
      target <- if lefted == this then upped.prev else Some(lefted)
      value <- target.last
    } yield value

    def next: Option[Loc] = for {
      upped <- this.up
      righted <- upped.right
      target <- if righted == this then upped.next else Some(righted)
      value <- target.first
    } yield value

    def left: Option[Loc] = (tree, context) match {
      case (Pair(l, r), c) => Some(Loc(l, L(c, r), depth + 1))
      case _               => None
    }
    def right: Option[Loc] = (tree, context) match {
      case (Pair(l, r), c) => Some(Loc(r, R(l, c), depth + 1))
      case _               => None
    }
    def up: Option[Loc] = (tree, context) match {
      case (t, L(c, tree)) => Some(Loc(Pair(t, tree), c, depth - 1))
      case (t, R(tree, c)) => Some(Loc(Pair(tree, t), c, depth - 1))
      case _               => None
    }
    def upmost: Loc = (tree, context) match {
      case (t, Context.Top) => this
      case _                => (this.up.get).upmost
    }

    def update(new_tree: Snailadic): Loc =
      Loc(new_tree, context, depth)

  }

  val number: Parsley[Snailadic.Number] =
    some(digit).map(x => Snailadic.Number(x.foldLeft(0)(_ + _.toString.toInt)))

  lazy val pair: Parsley[Snailadic.Pair] = for
    _ <- char('[')
    fst <- number <|> pair
    _ <- char(',')
    snd <- number <|> pair
    _ <- char(']')
  yield Snailadic.Pair(fst, snd)

  def parse(input: String): Seq[Snailadic] =
    input.split("\n").map(parse_line(_)).toSeq

  def parse_line(input: String): Snailadic =
    pair.parse(input) match {
      case Success(p) => p
      case Failure(e) => throw new Exception(e.toString)
    }
  val sample_input1 = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"""

  val sample_input2 = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""

  val input = """[[[[4,6],4],[1,7]],[[[1,6],[8,4]],[1,1]]]
[[[[8,5],[9,2]],1],[[2,5],[[9,4],[5,9]]]]
[[[[7,3],0],[8,9]],6]
[[6,[[7,2],[6,2]]],[[[9,8],9],[9,6]]]
[2,[[[9,2],6],[[5,3],[6,7]]]]
[[[5,[9,6]],0],[[[2,8],[7,0]],[7,[4,4]]]]
[[[[5,0],2],[0,1]],4]
[2,[8,8]]
[[[[2,5],[6,8]],[[9,8],4]],[[[2,3],[5,8]],[9,5]]]
[[[[0,7],[9,4]],[[1,0],9]],[[[8,8],[7,2]],[3,[6,5]]]]
[[[[3,2],8],1],[[4,[3,4]],[[6,5],[0,6]]]]
[[[7,8],8],[0,[5,2]]]
[[3,[3,3]],[[[6,9],[1,1]],[6,[2,9]]]]
[[[[9,7],[6,8]],4],[[[8,2],[2,9]],[8,[6,2]]]]
[[[[7,3],2],[9,6]],[[[1,7],[0,0]],[4,9]]]
[[8,[7,[1,0]]],7]
[[[7,[5,1]],0],[[8,[5,3]],4]]
[1,[[[2,6],2],[1,[6,0]]]]
[[[5,8],[[9,1],1]],[[3,[5,0]],5]]
[[[[1,5],[4,9]],8],[[7,0],6]]
[9,[[0,[1,0]],6]]
[[[[6,8],6],9],[[7,3],2]]
[[9,[[8,7],4]],[[[4,0],[9,0]],[8,1]]]
[[[2,[4,4]],[7,[0,1]]],[8,[[8,6],[4,0]]]]
[0,9]
[[[[1,8],[7,4]],[[5,0],[6,1]]],[5,7]]
[[[[8,2],[9,2]],[8,[8,4]]],[0,4]]
[[[[0,7],[5,8]],3],6]
[[[7,[3,4]],[3,[1,5]]],2]
[[[1,[4,2]],5],[[1,2],1]]
[[[[8,2],[0,9]],1],[[[9,0],[3,5]],[8,[8,0]]]]
[[[0,5],[1,[3,3]]],[[[1,0],[5,2]],[7,5]]]
[[[4,[7,3]],[0,9]],[[2,0],8]]
[[[[2,2],8],[7,1]],5]
[[1,[[3,8],7]],[[7,[5,8]],[4,[1,7]]]]
[[[[2,7],4],[8,[9,1]]],[[5,2],[4,3]]]
[[2,[7,2]],[[8,[0,8]],[0,[4,2]]]]
[[6,[6,[7,4]]],[[7,[2,0]],[[8,2],8]]]
[[[7,[1,7]],[[4,1],4]],[1,[4,6]]]
[1,[[1,0],[[0,3],[6,9]]]]
[[[[8,6],0],[[2,8],[3,0]]],[[[8,2],7],[[3,0],5]]]
[[[[2,8],4],[2,[0,7]]],[[3,[1,2]],[[8,0],[4,2]]]]
[1,8]
[[5,6],6]
[[[[1,0],[3,6]],[[4,0],1]],[0,7]]
[[[5,[9,6]],[7,[1,2]]],2]
[[[6,4],[[5,6],[1,8]]],[[[9,0],[7,7]],[[5,8],[6,8]]]]
[8,5]
[5,[[[6,8],8],0]]
[[[[5,7],[0,0]],[6,[0,0]]],[[[5,5],3],[[1,1],[3,4]]]]
[[[4,0],[[8,6],2]],[[3,[3,1]],[[2,8],[7,2]]]]
[[[8,7],[[5,5],[5,3]]],4]
[[[[5,4],1],[3,4]],[3,5]]
[[[6,5],[[6,3],6]],4]
[[[[2,2],[7,1]],[6,6]],[[8,[8,7]],[[1,6],[3,0]]]]
[[4,[[5,0],[7,4]]],[3,1]]
[[[3,[5,8]],5],[1,[[9,6],3]]]
[[0,[[3,0],[8,7]]],[[1,3],3]]
[5,[[3,[3,3]],[3,6]]]
[[[[7,3],8],3],[2,[[9,8],2]]]
[[[2,4],[[1,2],5]],[[[1,2],[6,0]],3]]
[[9,[[1,1],[1,7]]],[1,[2,[9,1]]]]
[[[5,[0,0]],5],[6,[0,1]]]
[[3,[[6,5],7]],[[7,8],3]]
[[5,[2,6]],8]
[[6,[0,[3,0]]],[1,2]]
[3,[[[3,7],2],[[4,0],6]]]
[[[8,[2,7]],[4,1]],[[2,[4,2]],3]]
[[3,2],[[[8,8],[8,6]],[[5,3],1]]]
[1,[2,[[3,2],[2,9]]]]
[8,[[9,1],[[8,4],[9,9]]]]
[[[4,[4,6]],[1,8]],[[7,7],[[7,4],3]]]
[[[8,2],[[9,7],[0,8]]],[[4,4],[[6,1],5]]]
[[[3,[6,6]],[[8,6],[3,7]]],[[7,9],[[5,3],8]]]
[[[8,9],[8,6]],[[[3,3],[2,9]],[[6,6],9]]]
[8,[[[3,0],5],2]]
[[[[1,3],1],[[1,9],4]],[7,[3,1]]]
[[[[9,3],3],[[6,8],7]],[[[2,0],3],[8,[3,6]]]]
[[[[7,1],[8,1]],[[4,6],[5,9]]],[[[4,5],3],5]]
[6,[[3,[0,0]],[6,6]]]
[[[[8,8],[7,6]],3],[[[7,7],[1,1]],[[1,8],[1,4]]]]
[[9,[8,[3,4]]],[[6,2],[1,5]]]
[[5,[3,3]],[5,[0,5]]]
[[[[8,9],5],[1,9]],[[5,[2,8]],[[6,4],[9,4]]]]
[2,6]
[[[[1,4],8],5],[5,[0,[1,7]]]]
[[[[1,0],[9,9]],[0,9]],[[[5,4],[1,6]],[9,[6,7]]]]
[[[7,1],5],[[3,2],5]]
[[9,[[8,8],[7,0]]],[5,[3,[1,3]]]]
[[[[5,2],[7,5]],[4,[6,7]]],[[[8,1],6],[2,[6,6]]]]
[[[5,7],[6,[8,2]]],[8,2]]
[[[[5,7],8],[[9,8],2]],[[2,8],[[7,6],3]]]
[[1,[[1,6],1]],[0,[[5,9],[9,1]]]]
[[[[1,4],[5,0]],[[5,5],[9,3]]],[[6,4],[4,[4,6]]]]
[7,[[5,[4,8]],[[5,9],2]]]
[[[[2,9],[1,8]],[4,2]],0]
[[5,[[0,9],[3,7]]],[2,[6,[4,8]]]]
[[0,[5,5]],0]
[[[5,0],[[0,5],8]],[6,[[8,7],[6,5]]]]
[[[5,[8,2]],[8,4]],[[6,2],[8,[7,0]]]]"""
}
