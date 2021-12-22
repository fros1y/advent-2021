import cats.effect.{IO, IOApp}
import scala.collection.mutable.Queue

@main
def solve_problem21: Unit = {
  import Problem21._
  val die = Die(0)
  val players = List(Player(1, Position(7)), Player(2, Position(3)))
  val game = Game(players)
  val (finalDie, finalGame) = game.play(die)
  println(finalDie)
  println(finalGame.players)
  println(finalDie.count * finalGame.players.map(_.score).min)

  val players_2 = List(
    Player(1, Position(7), win_score = 21),
    Player(2, Position(3), win_score = 21)
  )
  val game_2 = Game(players_2)
  val world = QuantumWorld(Map(game_2 -> 1))
  //val newWorld = world.step
  println("playing")
  val results = world.play
  println(results)
}
object Problem21 {

  case class Position(value: Int) {
    def move(step: Int): Position = Position(((value + step - 1) % 10) + 1)
    override def toString: String = value.toString
  }

  case class Player(
      id: Int,
      position: Position,
      score: Int = 0,
      win_score: Int = 1000
  ) {
    def move(steps: List[Int]): Player =
      val newPosition = steps.foldLeft(position)(_.move(_))
      val newScore = score + newPosition.value
      val newPlayer = Player(id, newPosition, newScore)
      println(s"$id: $this -> $newPlayer")
      newPlayer
    def winner: Boolean = score >= win_score
    override def toString: String = s"Player $id @ $position w/ score = $score"
  }

  case class QuantumWorld(
      val games: Map[Game, Int],
      val scores: Map[Int, Int] = Map()
  ) {
    def play: QuantumWorld = {
      def go(qw: QuantumWorld): QuantumWorld = {

        println("Simulating quantum world with " + qw.games.size + " games")
        val newWorld = qw.step
        val (finished, still_going) = newWorld.games.partition(_._1.hasWinner)
        println("Finished " + finished.size + " games")
        println("Still going " + still_going.size + " games")
        val newScores = finished.foldLeft(qw.scores)((acc, finish) => {
          val (game, count) = finish
          val newScore = acc.getOrElse(game.winner.get.id, 0) + count
          acc.updated(game.winner.get.id, newScore)
        })
        still_going.toSeq match {
          case Nil => QuantumWorld(Map(), newScores)
          case _   => go(QuantumWorld(still_going, newScores))
        }
      }
      go(this)
    }
    def step: QuantumWorld = {
      val rolls = for {
        a <- 1 to 3
        b <- 1 to 3
        c <- 1 to 3

      } yield List(a, b, c)

      val newGames = games.toSeq.flatMap { case (game, count) =>
        for possibleRoll <- rolls yield game.step(possibleRoll)._1 -> count
      }
      val counted = newGames.groupBy(_._1).mapValues(_.map(_._2).sum).toMap
      QuantumWorld(counted)
    }
    override def toString: String =
      games.toSeq.map { case (game, count) => s"$game: $count" }.mkString("\n")
  }
  case class Game(players: List[Player]) {

    def hasWinner: Boolean = players.exists(_.winner)

    def winner: Option[Player] = players.find(_.winner)

    def step(rolls: List[Int]): (Game, Option[Player]) = {
      val player = players.head
      val newPlayer = player.move(rolls)
      val winner = if newPlayer.winner then Some(newPlayer) else None
      val newPlayers = players.tail :+ newPlayer
      (Game(newPlayers), winner)
    }
    override def toString: String = "Game\n" + players.mkString("\n")
    def play(die: Die): (Die, Game) = {
      def go(game: Game, die: Die): (Die, Game) = {
        val (rolls, newDie) = die.newRolls
        val (newGame, winner) = game.step(rolls)
        winner match {
          case Some(player) => (newDie, newGame)
          case None         => go(newGame, newDie)
        }
      }
      go(this, die)
    }
  }

  case class Die(value: Int, max: Int = 100, count: Int = 0) {
    def newRoll: Die = {
      val newValue = (value % max) + 1
      Die(newValue, max, count + 1)
    }

    def newRolls: (List[Int], Die) = {
      val d1 = this.newRoll
      val d2 = d1.newRoll
      val d3 = d2.newRoll
      (List(d1.value, d2.value, d3.value), d3)
    }
  }
}
