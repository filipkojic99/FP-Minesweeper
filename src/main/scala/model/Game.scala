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
                      clicks: Int,
                      startedAtMs: Long,
                      endedAtMs: Option[Long]
                    ) {
  /** Get game duration in seconds. */
  def elapsedSeconds(nowMs: Long = System.currentTimeMillis()): Long =
    ((endedAtMs.getOrElse(nowMs) - startedAtMs) / 1000L).max(0L)
}

object GameState {
  /** Create a fresh game: everything hidden, status in progress, clicks = 0. */
  def newGame(board: Board): GameState = {
    val rows = board.rows
    val cols = board.cols
    val allHidden = Vector.fill(rows, cols)(CellState.Hidden)
    GameState(
      board        = board,
      state        = allHidden,
      status       = GameStatus.InProgress,
      clicks       = 0,
      startedAtMs  = System.currentTimeMillis(),  // start timer
      endedAtMs    = None
    )
  }
}
