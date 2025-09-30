import io.LevelIO
import logic.BoardOps
import logic.GameOps
import cli.Renderer
import model._

import scala.io.StdIn

object Main extends App {
  val path = "levels/beginner/level1.txt"

  // 1) Load board
  val raw = LevelIO.readLevel(path)
  val board = BoardOps.buildFromChars(raw)

  // 2) New game
  var gs = GameState.newGame(board)
  
  println("[DEBUG] Underlying board (mines & numbers):")
  Renderer.printBoardDebug(board)
  println()
  Renderer.printGame(gs)

  // 3) Loop
  var running = true
  while (running) {
    print("\nEnter command (L r c | D r c | Q): ")
    val line = StdIn.readLine()
    if (line == null) { running = false }
    else {
      val s = line.trim.toUpperCase
      if (s == "Q") running = false
      else {
        val parts = s.split("\\s+")
        if (parts.length == 3 && (parts(0) == "L" || parts(0) == "D")) {
          // user inputs 1-based; convert to 0-based
          val r = parts(1).toInt - 1
          val c = parts(2).toInt - 1
          parts(0) match {
            case "L" => gs = GameOps.reveal(gs, r, c)
            case "D" => gs = GameOps.toggleFlag(gs, r, c)
          }
          Renderer.printGame(gs)
          if (gs.status != GameStatus.InProgress) {
            println(s"\nGame over: ${gs.status}")
            println(s"Time:  ${gs.elapsedSeconds()}s")
            println(s"Clicks: ${gs.clicks}")
            println(s"Hints: ${gs.hintsUsed}")
            println(s"Score: ${gs.score.getOrElse("--")}")
            running = false
          }
        } else {
          println("Invalid command. Use: L r c | D r c | Q")
        }
      }
    }
  }
}
