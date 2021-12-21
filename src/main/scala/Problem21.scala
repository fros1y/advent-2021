import cats.effect.{IO, IOApp}
import scala.collection.mutable.Queue

@main
def solve_problem21: Unit = {
  import Problem21._
  val die = Die(100, 0, 0)
  val players = Queue(Player(1, 0, 7), Player(2, 0, 3))
  val qplayers = Queue(QPlayer(1, 0, 7), QPlayer(2, 0, 3))
  val game = Game(die, players)
  val (finalDie, finalPlayers) = game.play
  println(finalDie)
  println(finalPlayers)
  println(finalDie.count * finalPlayers.map(_.score).min)

  val qu = QuantumWorld(Map(QuantumGame(qplayers) -> 1))
  val results = qu.play
  println(results)
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
  case class QPlayer(id: Int, var score: Int, var position: Int) {
    def move(step: Int) = {
      position = ((position + step - 1) % 10) + 1
    }
    def addToScore = {
      println(s"$id: $score => ${score + position}")
      score += position
    }
    def isWinner: Boolean = score >= 21
    override def toString: String =
      s"Player $id @ $position with score of $score"
  }

  case class QuantumWorld(var universes: Map[QuantumGame, Int]) {
    def rolls = for
      a <- Seq(1, 2, 3)
      b <- Seq(1, 2, 3)
      c <- Seq(1, 2, 3)
    yield (a, b, c)

    def step = {
      val simulations: Seq[(QuantumGame, Int)] = for {
        roll <- rolls
        (g, c) <- universes.toSeq
      } yield {
        if g.winner then (g, c)
        else {
          var r = g.copy()
          r.step(roll)
          (r, c)
        }
      }
      println(simulations.mkString("\n\n"))
      val newUniverses = simulations.groupBy(_._1).mapValues(_.size)
      println("step")
      newUniverses
    }

    def play: Map[Int, Int] = {
      while (!universes.keys.forall(_.winner)) {
        step
      }
      universes = step.groupBy(_._1).mapValues(_.map(_._2).sum).toMap
      val counts = for {
        (g, c) <- universes
        pid <- g.playerWins
      } yield pid -> c
      val scores = counts.groupBy(_._1).mapValues(_.map(_._2).sum)
      scores.toMap
    }
  }
  case class QuantumGame(
      var players: Queue[QPlayer],
      var steps: Int = 0
  ) {
    def step(move: (Int, Int, Int)) = {
      var currPlayer = players.dequeue
      println(
        "Player " + currPlayer.id + " at " + currPlayer.position + " rolls " + move
      )
      currPlayer.move(move._1)
      currPlayer.move(move._2)
      currPlayer.move(move._3)
      println("Player " + currPlayer.id + " moves to " + currPlayer.position)
      currPlayer.addToScore
      println("Player " + currPlayer.id + " now has " + currPlayer.score)
      players.enqueue(currPlayer) //.enqueue
    }
    def winner = !players.forall(!_.isWinner)
    def noWinner = winner
    def playerWins = players.filter(_.isWinner).map(_.id)
    override def toString = "QuantumGame:\n" + players.mkString("\n")
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
