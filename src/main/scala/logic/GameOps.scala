package logic

import model.*

import scala.collection.immutable.Queue
import scala.language.postfixOps

object GameOps {

  /** Apply move. */
  def applyMove(gs: GameState, move: (Char, Int, Int)): GameState = move match {
    case ('L', r, c) => reveal(gs, r, c)
    case ('D', r, c) => toggleFlag(gs, r, c)
    case _ => gs
  }

  /** Hint - unflag misflagged field or click on empty field. */
  def computeHint(gs: GameState): Option[(Char, Int, Int)] = {
    if (gs.status != GameStatus.InProgress) return None
    val b = gs.board

    // misflagged
    val misflagged = for {
      r <- 0 until b.rows
      c <- 0 until b.cols
      if gs.state(r)(c) == CellState.Flagged
      if b.grid(r)(c).content == CellContent.Clear
    } yield ('D', r, c)

    if (misflagged.nonEmpty) return Some(misflagged.head)

    // zero
    val zeros = for {
      r <- 0 until b.rows
      c <- 0 until b.cols
      if gs.state(r)(c) == CellState.Hidden
      cell = b.grid(r)(c)
      if cell.content == CellContent.Clear && cell.adjacentMines == 0
    } yield ('L', r, c)

    if (zeros.nonEmpty) return Some(zeros.head)

    // any clear
    val clears = for {
      r <- 0 until b.rows
      c <- 0 until b.cols
      if gs.state(r)(c) == CellState.Hidden
      if b.grid(r)(c).content == CellContent.Clear
    } yield ('L', r, c)

    clears.headOption
  }

  /** Apply a hinted move and increment hintsUsed only when applied. */
  def applyHint(gs: GameState, moveOpt: Option[(Char, Int, Int)]): GameState = moveOpt match {
    case Some(('L', r, c)) =>
      reveal(gs.copy(hintsUsed = gs.hintsUsed + 1), r, c)
    case Some(('D', r, c)) =>
      toggleFlag(gs.copy(hintsUsed = gs.hintsUsed + 1), r, c)
    case _ =>
      gs
  }

  /** Left click: reveal a cell. If mine -> Lost. If 0 -> flood reveal. */
  def reveal(gs: GameState, row: Int, col: Int): GameState = {
    if (!inBounds(gs, row, col)) return gs
    if (gs.status != GameStatus.InProgress) return gs

    gs.state(row)(col) match {
      case CellState.Flagged => gs // left click on flagged does nothing
      case CellState.Revealed => gs
      case CellState.Hidden =>
        val cell = gs.board.grid(row)(col)
        cell.content match {
          case CellContent.Mine =>
            val stAfterMines = revealAllMines(gs.state, gs.board)
            gs.copy(
              state = stAfterMines,
              status = GameStatus.Lost,
              clicks = gs.clicks + 1,
              endedAtMs = Some(System.currentTimeMillis())
            )


          case CellContent.Clear =>
            val newState =
              if (cell.adjacentMines == 0) floodReveal(gs.state, gs.board, row, col)
              else setCellState(gs.state, row, col, CellState.Revealed)

            val newStatus = if (isWin(newState, gs.board)) GameStatus.Won else GameStatus.InProgress

            val now = System.currentTimeMillis()
            val ended = if (newStatus != GameStatus.InProgress) Some(now) else gs.endedAtMs

            val base = gs.copy(
              state = newState,
              status = newStatus,
              clicks = gs.clicks + 1,
              endedAtMs = ended
            )

            if (newStatus == GameStatus.Won)
              base.copy(score = Some(GameOps.computeScore(base, now)))
            else
              base
        }
    }
  }

  /** Right click: toggle flag on a Hidden cell; remove flag if Flagged. */
  def toggleFlag(gs: GameState, row: Int, col: Int): GameState = {
    if (!inBounds(gs, row, col)) return gs
    if (gs.status != GameStatus.InProgress) return gs

    gs.state(row)(col) match {
      case CellState.Revealed => gs // cannot flag revealed cells
      case CellState.Hidden =>
        val st = setCellState(gs.state, row, col, CellState.Flagged)
        gs.copy(state = st, clicks = gs.clicks + 1)
      case CellState.Flagged =>
        val st = setCellState(gs.state, row, col, CellState.Hidden)
        gs.copy(state = st, clicks = gs.clicks + 1)
    }
  }

  // ---------------- helpers ----------------

  /** Check if field is in bounds. */
  private def inBounds(gs: GameState, r: Int, c: Int): Boolean = {
    r >= 0 && r < gs.board.rows && c >= 0 && c < gs.board.cols
  }

  /** Set state of the cell - hidden, revealed, flagged . */
  private def setCellState(state: Vector[Vector[CellState]], r: Int, c: Int, to: CellState): Vector[Vector[CellState]] = {
    val row = state(r)
    val newRow = row.updated(c, to)
    state.updated(r, newRow)
  }

  /** Reveal all connected zeros plus their border numbers (classic flood). */
  private def floodReveal(state: Vector[Vector[CellState]], board: Board, startR: Int, startC: Int): Vector[Vector[CellState]] = {
    var st = state
    var q = Queue((startR, startC))

    def neighborsOf(r: Int, c: Int): Vector[(Int, Int)] = {
      val deltas = Vector(
        (-1,-1), (-1,0), (-1,1),
        ( 0,-1),         ( 0,1),
        ( 1,-1), ( 1,0), ( 1,1)
      )
      deltas.flatMap { case (dr, dc) =>
        val nr = r + dr; val nc = c + dc
        if (nr >= 0 && nr < board.rows && nc >= 0 && nc < board.cols) Some((nr, nc)) else None
      }
    }

    // reveal the starting cell if hidden/flagged
    st = setCellState(st, startR, startC, CellState.Revealed)

    while (q.nonEmpty) {
      val ((r, c), q2) = q.dequeue
      q = q2
      val cell = board.grid(r)(c)

      if (cell.adjacentMines == 0) {
        // all neighbors: reveal hidden ones; push zeros to queue
        neighborsOf(r, c).foreach { case (nr, nc) =>
          if (st(nr)(nc) == CellState.Hidden) {
            st = setCellState(st, nr, nc, CellState.Revealed)
            if (board.grid(nr)(nc).adjacentMines == 0) {
              q = q.enqueue((nr, nc))
            }
          }
        }
      }
    }
    st
  }

  /** Game over - reveal all mines */
  private def revealAllMines(state: Vector[Vector[CellState]], board: Board): Vector[Vector[CellState]] = {
    state.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (st, c) =>
        board.grid(r)(c).content match {
          case CellContent.Mine    => CellState.Revealed
          case CellContent.Clear   => st
        }
      }
    }
  }

  /** Win = all non-mine cells are revealed. */
  private def isWin(state: Vector[Vector[CellState]], board: Board): Boolean = {
    var totalClear = 0
    var revealedClear = 0
    for (r <- 0 until board.rows; c <- 0 until board.cols) {
      board.grid(r)(c).content match {
        case CellContent.Mine  => ()
        case CellContent.Clear =>
          totalClear += 1
          if (state(r)(c) == CellState.Revealed) revealedClear += 1
      }
    }
    revealedClear == totalClear
  }

  /** SCORE = game duration + number of clicks + number of hints * 10. */
  private def computeScore(gs: GameState, nowMs: Long = System.currentTimeMillis()): Int = {
    val secs = gs.elapsedSeconds(nowMs)
    (secs + gs.clicks + gs.hintsUsed * 10).toInt
  }
}
