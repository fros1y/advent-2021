import collection.{IndexedSeq, Iterable, Seq}
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
  space
}
import parsley.implicits.character.{charLift, stringLift}
import parsley.combinator._
import parsley.{Result, Success, Failure}
import parsley.debug.DebugCombinators

import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
import parsley.character.isWhitespace

@main
def run2b(): Unit = {
  val sample_input = """forward 5
down 5
forward 8
up 3
down 8
forward 2"""
  val input = """forward 8
down 9
forward 2
down 1
forward 9
forward 7
forward 5
up 3
up 3
down 5
forward 2
down 8
down 3
forward 6
down 2
down 4
down 7
down 7
forward 2
down 6
down 8
down 2
down 8
up 9
down 8
forward 8
down 5
up 4
forward 4
forward 4
forward 3
down 9
forward 8
up 3
forward 2
forward 3
forward 6
down 7
down 2
forward 4
forward 7
forward 6
up 5
up 1
forward 4
down 9
up 6
forward 6
up 9
forward 1
down 9
forward 4
down 3
forward 7
forward 5
down 1
up 9
down 9
forward 6
forward 1
down 5
down 6
forward 6
forward 3
up 4
up 9
down 3
forward 6
up 4
up 6
forward 4
down 1
down 2
up 9
forward 8
down 2
down 3
down 4
up 3
forward 3
forward 4
down 4
forward 7
forward 9
down 7
forward 6
forward 2
up 6
forward 7
forward 9
down 5
forward 6
up 9
forward 6
forward 2
forward 6
up 3
down 1
forward 5
down 3
forward 7
down 4
forward 1
forward 7
down 1
up 2
down 7
down 6
forward 8
forward 2
forward 1
forward 9
down 3
forward 3
down 6
up 8
up 3
forward 1
forward 3
forward 7
down 9
forward 7
forward 3
up 6
forward 4
down 9
forward 2
down 4
up 2
down 1
up 1
down 6
forward 1
up 6
up 7
forward 3
forward 3
forward 2
forward 1
down 7
forward 9
down 5
down 9
up 9
forward 3
forward 8
down 3
forward 9
forward 4
down 3
up 4
up 8
up 4
down 8
down 6
down 5
forward 2
up 6
up 1
up 9
down 4
up 8
forward 6
down 1
forward 7
up 2
forward 6
up 2
down 6
down 5
forward 2
down 2
down 1
forward 8
forward 1
up 9
forward 3
down 6
forward 2
forward 8
down 3
forward 3
forward 4
forward 7
forward 2
up 4
forward 8
forward 1
forward 9
down 3
down 1
forward 8
down 5
down 3
forward 5
down 7
down 1
forward 8
forward 2
forward 4
forward 8
forward 6
down 1
forward 5
forward 9
forward 9
up 9
forward 9
forward 4
down 5
down 2
down 3
forward 8
forward 9
up 8
up 1
up 6
forward 7
up 9
forward 2
forward 6
up 6
forward 3
up 4
forward 1
down 4
up 6
down 5
forward 7
forward 6
down 3
down 4
forward 3
down 6
down 1
forward 5
forward 7
up 8
forward 4
up 7
down 4
forward 3
down 7
forward 7
forward 4
forward 1
forward 8
up 5
up 6
forward 5
forward 3
down 6
forward 8
forward 2
forward 7
down 7
down 8
down 3
up 3
down 1
down 1
forward 6
forward 9
forward 4
forward 9
forward 6
down 1
forward 9
down 6
down 8
up 5
down 8
forward 4
forward 2
up 6
down 9
forward 6
down 9
down 6
down 6
forward 2
up 8
down 7
down 2
forward 2
forward 2
down 1
up 8
down 5
forward 9
forward 5
forward 8
forward 8
forward 1
down 2
down 7
up 5
forward 9
forward 4
forward 4
forward 6
down 7
up 5
forward 5
up 9
down 7
down 4
down 9
forward 7
up 4
down 1
down 6
up 2
up 6
down 2
forward 9
down 3
forward 3
forward 4
forward 1
up 2
forward 6
down 3
forward 2
down 9
forward 8
forward 3
forward 2
up 5
forward 3
forward 1
down 8
up 2
up 4
up 5
down 3
down 6
down 1
forward 4
up 3
down 1
down 4
up 6
forward 8
down 5
down 7
down 7
forward 9
forward 9
down 2
up 2
down 5
forward 5
forward 4
down 7
forward 4
down 2
forward 2
forward 4
up 8
down 8
up 4
forward 2
forward 2
up 8
forward 2
down 3
down 7
down 9
up 6
up 3
forward 2
forward 3
up 8
forward 6
up 8
up 1
down 6
down 8
up 9
down 1
up 8
forward 9
forward 4
forward 9
forward 8
forward 1
down 6
down 7
up 5
down 1
forward 9
down 9
forward 7
down 5
forward 7
down 1
down 4
down 4
forward 6
forward 1
up 4
up 2
forward 7
down 6
down 2
down 3
up 7
up 1
down 6
down 6
down 8
down 8
forward 5
forward 1
forward 5
up 8
forward 8
up 8
forward 1
down 9
forward 1
up 7
up 3
down 1
down 9
up 2
down 3
down 2
forward 9
up 9
up 1
up 5
forward 8
down 3
down 7
forward 7
down 8
down 5
down 5
down 4
down 7
forward 6
forward 6
forward 4
forward 6
forward 3
up 5
forward 2
down 7
forward 1
down 4
down 7
down 7
forward 4
down 2
up 4
forward 2
up 2
up 3
down 5
forward 3
down 2
forward 5
down 2
down 1
up 4
up 5
up 9
forward 1
forward 9
down 9
up 8
forward 9
forward 7
down 9
down 2
down 9
forward 9
forward 7
up 7
forward 6
up 6
forward 5
forward 6
down 4
forward 8
forward 5
forward 2
up 4
down 4
forward 1
down 2
up 9
up 7
up 2
up 3
down 9
forward 4
up 6
forward 5
forward 5
forward 9
forward 1
down 6
forward 8
down 5
up 3
up 1
down 2
up 4
down 1
down 9
forward 8
down 2
forward 7
down 7
up 5
down 7
down 3
forward 2
forward 2
forward 4
down 9
down 6
down 9
forward 6
down 6
down 9
forward 1
down 1
forward 8
down 4
up 6
down 2
forward 9
down 3
up 3
down 2
up 2
up 5
down 8
forward 1
forward 3
down 8
up 3
up 3
up 3
forward 2
up 3
down 4
down 1
down 7
forward 3
down 3
down 8
down 9
forward 9
forward 3
up 4
forward 8
forward 8
up 7
up 3
forward 6
down 9
up 1
forward 2
down 6
up 6
forward 2
down 6
down 3
up 7
forward 6
down 6
down 1
down 5
forward 6
up 2
down 2
down 3
down 1
up 9
forward 6
up 2
forward 2
down 6
up 3
up 4
forward 8
up 8
up 4
down 7
down 4
up 9
forward 9
up 6
forward 5
forward 7
forward 2
forward 8
down 7
down 5
down 4
up 3
forward 7
down 2
forward 5
forward 9
forward 4
forward 7
forward 8
up 6
down 1
forward 3
forward 9
forward 1
down 8
down 1
down 3
down 1
down 1
up 3
down 5
up 1
down 8
down 2
down 8
down 3
forward 2
forward 8
forward 4
down 8
down 6
forward 8
down 7
forward 8
forward 2
forward 6
forward 6
forward 4
up 2
forward 3
up 8
forward 7
forward 4
down 8
down 3
down 4
up 8
forward 5
forward 3
up 4
up 2
down 6
forward 4
up 8
up 3
up 8
down 3
up 1
forward 2
down 4
down 4
down 9
down 5
forward 9
up 6
up 5
down 8
down 6
down 7
forward 8
forward 4
up 4
forward 1
down 4
up 4
down 9
up 6
down 9
up 3
forward 4
down 1
down 5
down 5
up 1
down 8
down 9
down 1
forward 4
down 8
down 6
down 1
forward 2
down 5
up 1
up 1
down 1
down 3
down 3
down 8
down 6
down 5
down 3
up 3
forward 5
down 7
down 7
forward 2
forward 6
forward 1
down 8
forward 2
up 2
forward 2
down 2
forward 7
down 7
down 9
up 2
up 2
down 3
forward 1
down 1
forward 5
forward 4
down 4
forward 6
forward 2
forward 7
forward 2
forward 8
down 4
up 3
down 3
up 9
down 7
up 8
down 1
down 8
up 9
down 6
up 5
up 8
down 2
down 3
forward 1
down 5
down 5
forward 8
down 9
forward 7
forward 8
down 1
down 2
up 8
down 2
up 5
down 3
forward 5
forward 6
up 4
up 3
forward 5
forward 1
down 2
forward 2
down 9
up 7
down 2
up 8
forward 2
forward 2
up 4
forward 5
down 4
up 6
up 8
forward 9
up 1
forward 9
forward 7
forward 3
forward 1
down 1
forward 5
forward 2
forward 6
forward 6
forward 3
forward 7
up 7
down 6
down 2
up 5
down 5
up 2
forward 7
forward 2
down 9
up 4
down 5
down 3
forward 7
down 2
forward 9
forward 6
down 3
down 3
forward 6
down 9
forward 7
up 5
up 6
down 3
up 4
forward 4
up 3
down 8
down 9
down 9
down 4
up 1
up 2
down 3
forward 4
forward 5
down 1
up 3
down 8
down 7
forward 2
up 4
down 8
forward 1
forward 6
up 8
down 2
down 6
forward 5
up 8
forward 7
down 9
forward 9
down 3
forward 9
forward 1
forward 6
up 7
forward 1
down 5
down 8
down 3
forward 5
forward 6
down 7
forward 4
down 6
forward 4
up 4
down 4
forward 4
down 9
forward 3
down 6
forward 3
down 3
up 3
forward 3
down 4
down 4
down 4
down 5
forward 5
down 9
down 7
down 1
forward 5
forward 3
forward 8
down 5
forward 4
forward 3
down 8
forward 1
forward 1
down 6
forward 4
down 5
forward 8
down 7
forward 6
down 7
up 2
forward 6
down 7
down 3
down 6
up 2
forward 8
forward 5
down 6
forward 5
up 7
forward 8
down 1
forward 2
up 7
down 4
up 5
forward 5
forward 4
forward 3
down 1
forward 8
up 9
down 2
up 4
forward 7
up 6
forward 3
down 8
forward 4
down 2
down 9
forward 8
down 5
forward 3
down 9
up 1
forward 6
up 9
down 7
forward 5
forward 3
down 8
down 6
up 3
down 5
forward 7
up 2
forward 4
up 1
down 8
down 7
forward 3
forward 1
down 3
down 8
forward 7
up 2
down 7
up 6
down 7
down 1
down 3
up 6
forward 2
down 5
forward 1
forward 7
forward 2
down 8
down 8
down 7
forward 7
forward 5
forward 1
forward 7
forward 3
down 2
up 8
up 4
up 6
down 2
down 1
down 5
down 3
forward 2
up 1
forward 8
up 8
forward 7
up 1
down 7
forward 7
forward 5
forward 3
up 4
down 5
down 5
forward 3
forward 9
down 9
up 6
up 7
down 7
forward 7"""
  Problem2B.solve(input)
}

object Problem2B {

  case class Position(aim: Int = 0, depth: Int = 0, distance: Int = 0) {
    def move(c: Command): Position = c match {
      case Command.Forward(d) => Position(aim, depth - aim*d, distance + d)
      case Command.Up(d) => Position(aim + d, depth, distance)
      case Command.Down(d) => Position(aim - d, depth, distance)
    }
  }

  enum Command {
    case Forward(distance: Int)
    case Down(distance: Int)
    case Up(distance: Int)
  }

  val number = some(digit).map(_.mkString.toInt) 
  val forward = string("forward") //.debug("forward")
  val down = string("down") //.debug("down")
  val up = string("up") //.debug("up")
  

  val command_parser = choice(
    (forward ~> whitespace ~> number).map(Command.Forward(_)),
    (down ~> whitespace ~> number).map(Command.Down(_)),
    (up ~> whitespace ~> number).map(Command.Up(_)),
  )

  def parse_commands(input: String): Seq[Command] = {
    val parser = sepEndBy(command_parser, some(whitespace))
    parser.parse(input) match {
      case Success(commands) => commands
      case Failure(e) => throw new Exception(e.toString)
    }
  }

  def solve(input: String): Unit = {
    val commands = parse_commands(input)
    val position = commands.foldLeft(Position())((p, c) => p.move(c))
    println(position)
    println(position.distance * position.depth)
  }
}
