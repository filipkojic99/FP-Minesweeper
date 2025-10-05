package logic.gameOps
import logic.GameOps
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestBoards

class RevealSpec extends AnyFlatSpec with Matchers {

  "GameOps: reveal()" should "set status to Lost when mine is revealed" in {
    val gs = TestBoards.freshGame()

    val result = GameOps.reveal(gs, 0, 1)

    result.state(0)(1) shouldBe CellState.Revealed
    result.status shouldBe GameStatus.Lost
    result.clicks shouldBe 1
  }

  it should "reveal a single clear cell when it has adjacent mines" in {
    val gs = TestBoards.freshGame()

    val result = GameOps.reveal(gs, 0, 0)

    result.state(0)(0) shouldBe CellState.Revealed
    result.state(0)(1) shouldBe CellState.Hidden
    result.state(1)(0) shouldBe CellState.Hidden
    result.state(1)(1) shouldBe CellState.Hidden
    result.clicks shouldBe 1
    result.status shouldBe GameStatus.InProgress
  }

  it should "flood reveal all connected zero cells and borders" in {
    val gs = TestBoards.freshGame4x4()

    val result = GameOps.reveal(gs, 1, 0)

    val expected = Vector(
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Hidden),
    )

    result.clicks shouldBe 1
    result.state shouldBe expected
    result.status shouldBe GameStatus.InProgress
  }

  it should "reveal a single cell if it has neighbour Mines" in {
    val gs = TestBoards.freshGame()

    val revealed = GameOps.reveal(gs, 1, 0)

    val expected = gs.copy(state = Vector(
      Vector(CellState.Hidden, CellState.Hidden),
      Vector(CellState.Revealed, CellState.Hidden)
    ))

    expected.state shouldBe revealed.state
    expected.status shouldBe revealed.status
  }

  it should "change status to Won if last Clear cell is revealed" in {
    val gs = TestBoards.freshOneClearGame()

    val result = GameOps.reveal(gs, 0, 0)

    result.state(0)(0) shouldBe CellState.Revealed
    result.state(0)(1) shouldBe CellState.Flagged
    result.status shouldBe GameStatus.Won
    result.clicks shouldBe 1
  }

  it should "mark all mines and compute score when last Clear cell is revealed (4x4 board)" in {
    val gs = TestBoards.almostWonGame4x4()

    val result = GameOps.reveal(gs, 1, 3)

    val revealed = Vector(
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Flagged),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Revealed),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Revealed),
      Vector(CellState.Revealed, CellState.Revealed, CellState.Revealed, CellState.Flagged)
    )

    result.status shouldBe GameStatus.Won

    result.state shouldBe revealed

    val ended = result.endedAtMs.get
    val elapsedSec = ((ended - gs.startedAtMs) / 1000L).max(0L) + gs.elapsedSavedSec
    val expectedScore = (elapsedSec + (gs.clicks + 1) + gs.hintsUsed * 10).toInt
    result.score should contain(expectedScore)

    result.clicks shouldBe gs.clicks + 1
    result.endedAtMs should not be None
  }


  it should "do nothing when cell is Flagged" in {
    val gs = TestBoards.freshGame()

    val flagged = gs.copy(state = Vector(
      Vector(CellState.Hidden, CellState.Flagged),
      Vector(CellState.Hidden, CellState.Hidden)
    ))

    val result = GameOps.reveal(flagged, 0, 1)

    result.state shouldBe flagged.state
    result.clicks shouldBe flagged.clicks
    result.status shouldBe flagged.status
  }

  it should "do nothing when cell is already Revealed" in {
    val gs = TestBoards.freshGame()

    val revealed = gs.copy(state = Vector(
      Vector(CellState.Revealed, CellState.Hidden),
      Vector(CellState.Hidden, CellState.Hidden)
    ))

    val result = GameOps.reveal(revealed, 0, 0)

    result.state shouldBe revealed.state
    result.clicks shouldBe revealed.clicks
    result.status shouldBe revealed.status
  }

  it should "do nothing when game is not InProgress" in {
    val gs = TestBoards.freshGame().copy(status = GameStatus.Won)
    val res = GameOps.reveal(gs, 0, 0)
    res shouldBe gs
  }

  it should "ignore out-of-bounds coordinates" in {
    val gs = TestBoards.freshGame()
    val res = GameOps.reveal(gs, 2, -1)
    res shouldBe gs
  }

}
