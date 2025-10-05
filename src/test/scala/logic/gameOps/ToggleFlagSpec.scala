package logic.gameOps

import logic.GameOps
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestBoards

class ToggleFlagSpec extends AnyFlatSpec with Matchers {

  "GameOps: toggleFlag()" should "flag a hidden cell" in {
    val gs = TestBoards.freshGame()

    val updated = GameOps.toggleFlag(gs, 0, 1)

    updated.state(0)(1) shouldBe CellState.Flagged
    updated.clicks shouldBe 1
  }

  it should "unflag an already flagged cell" in {
    val gs = TestBoards.freshGame()
    val flagged = GameOps.toggleFlag(gs, 0, 1)

    flagged.state(0)(1) shouldBe CellState.Flagged

    val unflagged = GameOps.toggleFlag(flagged, 0, 1)

    unflagged.state(0)(1) shouldBe CellState.Hidden
    unflagged.clicks shouldBe 2
  }

  it should "not flag a revealed cell" in {
    val gs = TestBoards.freshGame()
    val revealed = gs.copy(state = Vector(
      Vector(CellState.Revealed, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Hidden)
    ))

    val result = GameOps.toggleFlag(revealed, 0, 0)

    result.state(0)(0) shouldBe CellState.Revealed
    result.clicks shouldBe 0
  }

  it should "do nothing when game is not InProgress" in {
    val gs = TestBoards.freshGame().copy(status = GameStatus.Won)
    val res = GameOps.toggleFlag(gs, 0, 1)
    res shouldBe gs
  }

  it should "ignore out-of-bounds coordinates" in {
    val gs = TestBoards.freshGame()
    val res = GameOps.toggleFlag(gs, 2, -1)
    res shouldBe gs
  }
  
}
