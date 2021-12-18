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
  val parsed = parse(sample_input)
//   val test_case = parsed(1)
//   println(test_case)
//   val zipper = Loc(test_case, Context.Top, 0)
//   println(zipper.tree)
//   println(zipper.left.get.tree)
//   println(zipper.left.get.right.get.tree)
// //  println(zipper.left.get.right.get.next.get.tree)
//   println(
//     zipper.left.get.right.get
//       .update(Snailadic.Number(6))
//       .upmost
//       .tree
//  )

  for e <- parse("""[[[[[9,8],1],2],3],4]
[7,[6,[5,[4,[3,2]]]]]
[[6,[5,[4,[3,2]]]],1]
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]""") do {
    println(e)
    val e_step = e.explode_step
    println("\t\t\t" + e_step)
    println()

  }

}
object Problem18 {

  enum Snailadic {
    case Number(x: Int)
    case Pair(fst: Snailadic, snd: Snailadic)

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
    def reduce: Snailadic = this
    def explode_step: Option[Snailadic] = {
      for {
        target <- Loc(this, Context.Top, 0).firstExplodePair
        _ = println("target: " + target)
        left <- target.left
        left_value <- left.tree.getNumber
        right <- target.right
        right_value <- right.tree.getNumber
        new_target = target.update(Number(0))
        _ = println("new_target: " + new_target.upmost.tree)

        lefted_updated = (for {
          update_location <- new_target.prev
          _ = println("left_update_location: " + update_location)
          orig_value <- update_location.tree.getNumber
          _ = println("left_update value: " + orig_value)
          updated = update_location.update(Number(left_value + orig_value))
          restored <- updated.next
        } yield restored).getOrElse(new_target)

        _ = println("lefted_updated: " + lefted_updated.upmost.tree)

        righted_updated = (for {
          update_location <- lefted_updated.next
          _ = println("right_update_location: " + update_location)
          orig_value <- update_location.tree.getNumber
          _ = println("right_update value: " + orig_value)
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
    // def firstPairWhere(f: Loc => Boolean): Option[Loc] = {
    //   println("firstPairWhere: " + this)
    //   if this.tree.isPair && f(this) then Some(this)
    //   else if this.nextPair.isDefined then this.nextPair.get.firstPairWhere(f)
    //   else None
    // }
    // //   then this.up.get.firstWhere(f)
    //   else if f(this) then Some(this)
    //   else {
    //     val next = this.next
    //     println("next: " + next)
    //     next match {
    //       case Some(x) => x.firstWhere(f)
    //       case None    => None
    //     }
    //   }
    // }
    // def nextPair: Option[Loc] = {
    //   (context, tree) match {
    //     case (Top, Number(_))                  => None
    //     case (Top, Pair(Number(_), Number(_))) => Some(this)
    //     case (Top, Pair(Pair(_, _), _))        => this.left.get.nextPair
    //     case (Top, Pair(Number(_), r))         => this.right.get.nextPair
    //     case (_, Number(_))                    => this.up.get.nextPair
    //     case (_, Pair(_, _))                   => this.up.get.nextPair

    //   }
    // }

    def firstExplodePair: Option[Loc] =
      println("fep: " + tree + " depth: " + this.depth)
      tree match {
        case Number(_)                                    => None
        case Pair(Number(_), Number(_)) if this.depth > 3 => Some(this)
        case Pair(Number(_), Number(_))                   => None
        //case Pair(Number(_), r) => this.right.get.firstExplodePair
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

  //   case (Number(_), Top) => this
  //   case (Number(_), L(_, _)) => this
  //   case (Number(_), R(_, _)) => this
  //   case (Pair(fst, snd), Top) => Loc(fst, L(Top, fst))
  //   case (Pair(fst, snd), L(ctx, left)) => Loc(fst, L(ctx, fst))
  //   case (Pair(fst, snd), R(right, ctx)) => Loc(snd, R(snd, L(ctx, snd)))
  // }

  // enum Action {
  //   case AddLeft(x: Int)
  //   case AddRight(x: Int)
  //   case Split
  // }
  // object Snailadic {

  //   def reduce(
  //       snailadic: Snailadic,
  //       depth: Int = 0
  //   ): (Snailadic, Set[Action]) =
  //     if depth >= 3 then {
  //       snailadic match {
  //         case Snailadic.Number(x) => (Snailadic.Number(x), Set.empty)
  //         case Snailadic.Pair(Snailadic.Number(a), Snailadic.Number(b)) => (Snailadic.Number(0), Set(AddLeft(a), AddRight(b)))
  //         case Snailadic.Pair(Snailadic.Number(a), Snailadic.Pair(right)) =>

  //          if depth == 3 =>
  //         (Snailadic.Pair(Snailadic.Number(0),  )), Set.empty)

  //       //   val (a1, acts_a) = reduce(a, depth + 1)
  //       //   val (b1, acts_b) = reduce(b, depth + 1)
  //       //   (Snailadic.Pair(a1, b1), (l, r1))
  //       // case Snailadic.Number(x) if x >= 10 =>
  //       //   (
  //       //     Pair(
  //       //       Snailadic.Number((x.toDouble / 2.0).floor.toInt),
  //       //       Snailadic.Number((x.toDouble / 2.0).ceil.toInt)
  //       //     ),
  //       //     Set()
  //       //   )

  //     }
  //   def +(a: Snailadic, b: Snailadic): Snailadic =
  //     val initial = Snailadic.Pair(a, b)
  //     reduce(initial)
  // }

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
  val sample_input = """[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"""
}
