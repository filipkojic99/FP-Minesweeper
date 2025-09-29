package model

enum CellState {
  case Hidden, Revealed, Flagged
}

enum GameStatus {
  case InProgress, Won, Lost
}

case class GameState(
                      board: Board,
                      state: Vector[Vector[CellState]],
                      status: GameStatus,
                      clicks: Int
                    )

object GameState {
  /** Create a fresh game: everything hidden, status in progress, clicks = 0. */
  def newGame(board: Board): GameState = {
    val rows = board.rows
    val cols = board.cols
    val allHidden = Vector.fill(rows, cols)(CellState.Hidden)
    GameState(board, allHidden, GameStatus.InProgress, 0)
  }
}
