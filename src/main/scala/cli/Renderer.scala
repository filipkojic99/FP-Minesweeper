package cli

import model._

object Renderer {

  def printBoard(board: Board): Unit = {
    board.grid.foreach { row =>
      val line = row.map {
        case Cell(CellContent.Mine, _)    => "*"
        case Cell(CellContent.Clear, adj) => adj.toString
      }.mkString(" ")
      println(line)
    }
  }
}
