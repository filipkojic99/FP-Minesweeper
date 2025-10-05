package logic.gameOps

import logic.GameOps
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestBoards

class ApplyHintSpec extends AnyFlatSpec with Matchers {

  "GameOps: applyHint()" should "call reveal and increment hintsUsed when hint is ('L', r, c)" in {
    val gs = TestBoards.freshGame()
    val move = Some(('L', 0, 0))

    val result = GameOps.applyHint(gs, move)

    result.state(0)(0) shouldBe CellState.Revealed
    result.hintsUsed shouldBe gs.hintsUsed + 1
  }

  it should "call toggleFlag and increment hintsUsed when hint is ('D', r, c)" in {
    val gs = TestBoards.freshGame()
    val move = Some(('D', 0, 1))

    val result = GameOps.applyHint(gs, move)

    result.state(0)(1) shouldBe CellState.Flagged
    result.hintsUsed shouldBe gs.hintsUsed + 1
  }

  it should "return the same GameState and not increment hintsUsed for None" in {
    val gs = TestBoards.freshGame()
    val result = GameOps.applyHint(gs, None)

    result shouldBe gs
    result.hintsUsed shouldBe gs.hintsUsed
  }
}
