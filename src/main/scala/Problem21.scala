import cats.effect.{IO, IOApp}
import scala.collection.mutable.Queue

@main
def solve_problem21: Unit = {
  import Problem21._
  val die = Die(100, 0, 0)
  val players = Queue(Player(1, 0, 7), Player(2, 0, 3))
  val game = Game(die, players)
  val (finalDie, finalPlayers) = game.play
  println(finalDie)
  println(finalPlayers)
  println(finalDie.count * finalPlayers.map(_.score).min)
}
object Problem21 {
  case class Die(max: Int, var value: Int, var count: Int) {
    def roll: Int = {
      value = (value + 1) % max
      count = count + 1
      value
    }
    def roll3: (Int, Int, Int) = {
      val d1 = roll
      val d2 = roll
      val d3 = roll
      (d1, d2, d3)
    }
  }

  case class Player(id: Int, var score: Int, var position: Int) {
    def move(step: Int) = {
      position = ((position + step - 1) % 10) + 1
    }
    def addToScore = {
      score += position
    }
    def isWinner: Boolean = score >= 1000
  }

  case class Game(
      var die: Die,
      var players: Queue[Player],
      var steps: Int = 0
  ) {
    def winner = !players.forall(!_.isWinner)
    def step = {
      var currPlayer = players.dequeue
      currPlayer.move(die.roll)
      currPlayer.move(die.roll)
      currPlayer.move(die.roll)
      currPlayer.addToScore
      players.enqueue(currPlayer)
    }
    def play: (Die, Seq[Player]) = {
      while (!winner) {
        step
      }
      (die, players.toSeq)
    }
  }
}
