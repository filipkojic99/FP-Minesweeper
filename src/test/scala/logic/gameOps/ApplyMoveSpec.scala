package logic.gameOps

import logic.GameOps
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestBoards

class ApplyMoveSpec extends AnyFlatSpec with Matchers {

  "GameOps: applyMove()" should "delegate to reveal when move is ('L', r, c)" in {
    val gs = TestBoards.freshGame()
    val result = GameOps.applyMove(gs, ('L', 0, 0))

    result.state(0)(0) shouldBe CellState.Revealed
  }

  it should "delegate to toggleFlag when move is ('D', r, c)" in {
    val gs = TestBoards.freshGame()
    val result = GameOps.applyMove(gs, ('D', 0, 1))

    result.state(0)(1) shouldBe CellState.Flagged
  }

  it should "return the same GameState for invalid move character" in {
    val gs = TestBoards.freshGame()
    val result = GameOps.applyMove(gs, ('X', 0, 0))

    result shouldBe gs
  }
}
