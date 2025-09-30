package io

import model._

import java.io.PrintWriter

object GameIO {
  /** Save current game state into a file. */
  def save(path: String, gs: GameState, levelPath: String): Unit = {
    if (gs.status != GameStatus.InProgress) {
      throw new IllegalStateException("Cannot save a finished game.")
    }

    val pw = new PrintWriter(path)
    try {
      pw.println(s"LEVEL=$levelPath")
      pw.println(s"STATUS=${gs.status}")
      pw.println(s"CLICKS=${gs.clicks}")
      pw.println(s"STARTED=${gs.startedAtMs}")
      pw.println(s"HINTS=${gs.hintsUsed}")

      // save cell state grid as lines of characters (H=Hidden, R=Revealed, F=Flagged)
      for (row <- gs.state) {
        val line = row.map {
          case CellState.Hidden   => 'H'
          case CellState.Revealed => 'R'
          case CellState.Flagged  => 'F'
        }.mkString
        pw.println(line)
      }
    } finally {
      pw.close()
    }
  }
}
