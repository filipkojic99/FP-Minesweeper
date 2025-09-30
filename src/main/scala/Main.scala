import io.{GameIO, LevelIO, MoveIO}
import logic.{BoardOps, GameOps}
import cli.Renderer
import model.*

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
    print("\nEnter command (L r c | D r c | H | PLAY path | SAVE path | Q): ")
    val line = StdIn.readLine()
    if (line == null) {
      running = false
    } else {
      val s = line.trim
      s.toUpperCase match {
        case "Q" =>
          running = false

        case "H" =>
          val hint = GameOps.computeHint(gs) // Option[(Char, Int, Int)]
          hint match {
            case Some((kind, r, c)) =>
              val kindStr = if (kind == 'L') "Left-click" else "Unflag"
              println(s"Hint: $kindStr at (${r + 1}, ${c + 1})")
              gs = GameOps.applyHint(gs, hint) // apply & increment hintsUsed
              Renderer.printGame(gs)
            case None =>
              println("Hint: no move available")
          }
          if (gs.status != GameStatus.InProgress) {
            println(s"\nGame over: ${gs.status}")
            println(s"Time:  ${gs.elapsedSeconds()}s")
            println(s"Clicks: ${gs.clicks}")
            println(s"Hints: ${gs.hintsUsed}")
            println(s"Score: ${gs.score.getOrElse("--")}")
            running = false
          }

        case up if up.startsWith("PLAY ") =>
          val mPath = s.stripPrefix("PLAY").trim
          try {
            val moves = MoveIO.readMoves(mPath)
            println(s"Playing ${moves.length} moves from $mPath...\n")
            moves.zipWithIndex.foreach { case (mv, i) =>
              println(s"Move ${i + 1}: ${MoveIO.pretty(mv)}")
              gs = GameOps.applyMove(gs, mv)
              Renderer.printGame(gs)
              if (gs.status != GameStatus.InProgress) {
                println(s"\nGame over: ${gs.status}")
                println(s"Time:  ${gs.elapsedSeconds()}s")
                println(s"Clicks: ${gs.clicks}")
                println(s"Hints: ${gs.hintsUsed}")
                println(s"Score: ${gs.score.getOrElse("--")}")
                running = false
              }
            }
          } catch {
            case ex: Exception =>
              println(s"Failed to play moves from '$mPath': ${ex.getMessage}")
          }

        case cmd if cmd.startsWith("SAVE ") =>
          val savePath = cmd.stripPrefix("SAVE").trim
          try {
            GameIO.save(savePath, gs, path)
            println(s"Game saved to $savePath")
          } catch {
            case ex: Exception =>
              println(s"Failed to save: ${ex.getMessage}")
          }

        case _ =>
          val parts = s.split("\\s+")
          if (parts.length == 3 && (parts(0) == "L" || parts(0) == "D")) {
            val r = parts(1).toInt - 1
            val c = parts(2).toInt - 1
            gs = parts(0) match {
              case "L" => GameOps.reveal(gs, r, c)
              case "D" => GameOps.toggleFlag(gs, r, c)
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
            println("Invalid command. Use: L r c | D r c | H | PLAY path | Q")
          }
      }
    }
  }
}
