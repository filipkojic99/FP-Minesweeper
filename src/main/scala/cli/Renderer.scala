package cli

import model._

object Renderer {

  /** Player view: Hidden = ■, Flagged = F, Revealed = number / * (if lost) */
  def printGame(gs: GameState): Unit = {
    val b = gs.board
    println(s"Status: ${gs.status} | Clicks: ${gs.clicks} | Hints used: ${gs.hintsUsed} | Time elapsed:" +
      s" ${gs.elapsedSeconds()}s | Size: ${b.rows}x${b.cols}")
    for (r <- 0 until b.rows) {
      val line = (0 until b.cols).map { c =>
        gs.state(r)(c) match {
          case CellState.Hidden => "■"
          case CellState.Flagged => "F"
          case CellState.Revealed =>
            b.grid(r)(c).content match {
              case CellContent.Mine => "*" // if revealed (loss) show mine
              case CellContent.Clear =>
                val n = b.grid(r)(c).adjacentMines
                n.toString
            }
        }
      }.mkString(" ")
      println(line)
    }
  }

  /** Dev/debug view: mines as *, numbers for clears. */
  def printBoardDebug(b: Board): Unit = {
    for (r <- 0 until b.rows) {
      val line = (0 until b.cols).map { c =>
        b.grid(r)(c).content match {
          case CellContent.Mine => "*"
          case CellContent.Clear => b.grid(r)(c).adjacentMines.toString
        }
      }.mkString(" ")
      println(line)
    }
  }

  /** Prints level - # = Mine, - = Clear. */
  def printLevel(level: Level): Unit = {
    println(s"Level: ${level.rows} x ${level.cols}")
    level.cells.foreach { row =>
      val line = row.map {
        case CellContent.Mine => "#"
        case CellContent.Clear => "-"
      }.mkString(" ")
      println(line)
    }
  }
}
