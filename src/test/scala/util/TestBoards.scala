package util

import model.{Cell, *}

object TestBoards {

  def freshGame(): GameState = GameState.newGame(TestBoards.simpleBoard())

  def freshGame4x4(): GameState = GameState.newGame(TestBoards.Board4x4())

  def freshOneClearGame(): GameState = GameState.newGame(oneClearBoard())

  def almostWonGame4x4(): GameState = {
    val board = Board4x4()

    val preWinState = Vector(
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Flagged),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Revealed),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Flagged)
    )

    GameState(
      board = board,
      state = preWinState,
      status = GameStatus.InProgress,
      clicks = 6,
      startedAtMs = System.currentTimeMillis() - 45000, //game lasting 45s
      endedAtMs = None,
      hintsUsed = 2, // 2 hints
      score = None,
      elapsedSavedSec = 10
    )
  }


  private def simpleBoard(): Board = Board(Vector(
    Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0)),
    Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1))
  ))

  private def Board4x4(): Board = Board(Vector(
    Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0)),
    Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1)),
    Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1)),
    Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0))
  ))

  private def oneClearBoard(): Board = Board(Vector(
    Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Mine, 0))
  ))

}
