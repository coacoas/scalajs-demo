package presentation

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{React, ReactComponentB}
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode

import scala.collection.mutable
import scala.scalajs.js._
import scala.scalajs.js.annotation.JSExport
import scala.util.Random


case class Player(x: Int, y: Int, alive: Boolean = true)
case class Enemy(x: Int, y: Int)
case class Game(width: Int, height: Int, player: Player, enemies: List[Enemy])
case class Move(dx: Int, dy: Int) {
  def +(other: Move) = Move(dx + other.dx, dy + other.dy)
}

@JSExport
class Demo extends JSApp {
  val validMoves: Map[Int, Move] = Map(
    KeyCode.Up -> Move(0, -4),
    KeyCode.Down -> Move(0, 4),
    KeyCode.Left -> Move(-4, 0),
    KeyCode.Right -> Move(4, 0))

  override def main() = {
    val attach = document.getElementById("container")
    val (w, h) = (512, 384)
    val initPlayer = Player(w / 2, h / 2)
    val initEnemies = List.fill(20)(Enemy(x = Random.nextInt(w), y = Random.nextInt(h)))
    val initGame = Game(width = w, height = h, player = initPlayer, enemies = initEnemies)

    val moves = mutable.Set[Move]()
    document.onkeydown = (e: KeyboardEvent) => validMoves.get(e.keyCode).foreach(moves.add)
    document.onkeyup = (e: KeyboardEvent) => validMoves.get(e.keyCode).foreach(moves.remove)

    def render(game: Game): Unit = {
      val enemyStep = game.enemies.map(e => e.copy(y = (e.y + 2) % game.height))
      val playerStep = move(moves, game.player, enemyStep).getOrElse(game.player)
      val gameStep = game.copy(player = checkAlive(playerStep, enemyStep), enemies = enemyStep)
      React.render(Components.game(gameStep), attach)

      if (playerStep.alive)
        setTimeout(() => render(gameStep), 33)
    }

    render(initGame)
  }

  def checkAlive(player: Player, enemies: List[Enemy]): Player =
    player.copy(alive = enemies.forall { e =>
      Math.abs(e.x - player.x) >= 10 || Math.abs(e.y - player.y) >= 10
    })

  def move(moves: mutable.Set[Move], player: Player, enemies: List[Enemy]) = for {
    move <- moves.reduceOption(_ + _)
    x     = player.x + move.dx
    y     = player.y + move.dy
  } yield player.copy(x = x, y = y)
}

object Components {
  val enemy = ReactComponentB[Enemy]("enemy").render { enemy =>
    <.svg.circle(
      ^.svg.fill := "red",
      ^.svg.cx := enemy.x,
      ^.svg.cy := enemy.y,
      ^.svg.r := 5).render
  }.build

  val player = ReactComponentB[Player]("player").render { player =>
    val color = if (player.alive) "white" else "gray"
    <.svg.circle(
      ^.svg.fill := color,
      ^.svg.cx := player.x,
      ^.svg.cy := player.y,
      ^.svg.r := 5).render
  }.build

  val game = ReactComponentB[Game]("game").render { game =>
    <.svg.svg(
      ^.background := "black",
      ^.width := game.width,
      ^.height := game.height,
      player(game.player),
      game.enemies.map(enemy(_))
    ).render
  }.build
}
