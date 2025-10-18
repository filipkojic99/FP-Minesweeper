package io

import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IOSpec extends AnyFlatSpec with Matchers {

  "LevelIO: readLevel()" should "read the levels/beginner/level1.txt file correctly" in {
    val path = "levels/beginner/level1.txt"

    val level = LevelIO.readLevel(path)

    level.length shouldBe 3
    level.foreach(_.length shouldBe 5)

    level(0) shouldBe Vector('-', '-', '-', '#', '-')
    level(1) shouldBe Vector('-', '-', '#', '-', '-')
    level(2) shouldBe Vector('-', '-', '-', '-', '-')
  }

  "MoveIO: readMoves()" should "parse moves moves/demo.txt file correctly" in {
    val path = "moves/demo.txt"

    val moves = MoveIO.readMoves(path)

    moves shouldBe Vector(
      ('L', 0, 0),
      ('D', 0, 1),
      ('L', 1, 1),
      ('L', 2, 4)
    )
  }

  "GameIO.save/load" should "save and load a simple game correctly" in {
    val board = Board(Vector(
      Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Mine, 0)),
      Vector(Cell(CellContent.Clear, 1), Cell(CellContent.Clear, 1))
    ))

    val state = Vector(
      Vector(CellState.Hidden, CellState.Flagged),
      Vector(CellState.Revealed, CellState.Hidden)
    )

    val gs = GameState(
      board = board,
      state = state,
      status = GameStatus.InProgress,
      clicks = 4,
      startedAtMs = System.currentTimeMillis() - 5000,
      endedAtMs = None,
      hintsUsed = 2,
      score = None,
      elapsedSavedSec = 12
    )

    val path = "src/test/resources/test.txt"

    GameIO.save(path, gs, "src/test/resources/testIO.txt")

    val (loaded, levelPath) = GameIO.load(path)

    levelPath shouldBe "src/test/resources/testIO.txt"
    loaded.status shouldBe gs.status
    loaded.clicks shouldBe gs.clicks
    loaded.hintsUsed shouldBe gs.hintsUsed
    loaded.state shouldBe gs.state
    loaded.board.rows shouldBe 2
    loaded.board.cols shouldBe 2
    loaded.score shouldBe None
    loaded.endedAtMs shouldBe None
  }
}
