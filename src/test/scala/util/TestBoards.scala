package util

import model.*

object TestBoards {

  def simpleBoard(): Board = Board(Vector(
    Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0)),
    Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1))
  ))

  def freshGame(): GameState = GameState.newGame(TestBoards.simpleBoard())
}
