package logic.gameOps

import logic.GameOps
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestBoards

class ComputeHintSpec extends AnyFlatSpec with Matchers {

  "GameOps: computeHint()" should "return ('D', r, c) for a misflagged clear cell" in {
    val board = Board(Vector(
      Vector(Cell(CellContent.Mine, 0), Cell(CellContent.Clear, 1))
    ))
    val state = Vector(
      Vector(CellState.Hidden, CellState.Flagged)
    )
    val gs = TestBoards.mkGame(board, state)

    GameOps.computeHint(gs) shouldBe Some(('D', 0, 1))
  }

  it should "return ('L', r, c) for the first zero-adjacent clear cell when no misflags" in {
    val board = Board(Vector(
      Vector(Cell(CellContent.Clear, 0), Cell(CellContent.Clear, 0)),
      Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1)),
      Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0))
    ))
    val state = Vector.fill(3, 2)(CellState.Hidden)
    val gs = TestBoards.mkGame(board, state)

    GameOps.computeHint(gs) shouldBe Some(('L', 0, 0))
  }

  it should "return ('L', r, c) for the first non-zero clear cell when no zeros exist" in {
    val board = Board(Vector(
      Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1)),
      Vector(Cell(CellContent.Mine, 0), Cell(CellContent.Clear, 2)),
      Vector(Cell(CellContent.Mine, 0), Cell(CellContent.Clear, 2))
    ))
    val state = Vector(
      Vector(CellState.Revealed, CellState.Revealed),
      Vector(CellState.Hidden, CellState.Hidden),
      Vector(CellState.Hidden, CellState.Hidden)
    )
    val gs = TestBoards.mkGame(board, state)

    GameOps.computeHint(gs) shouldBe Some(('L', 1, 1))
  }

  it should "return None when game is not InProgress" in {
    val board = Board(Vector(Vector(Cell(CellContent.Clear, 0))))
    val state = Vector(Vector(CellState.Hidden))
    val gs = TestBoards.mkGame(board, state, status = GameStatus.Won)

    GameOps.computeHint(gs) shouldBe None
  }
}
