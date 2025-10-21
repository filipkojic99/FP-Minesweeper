package io

import gui.services.ScoresFs
import logic.level.isometries.Iso
import model.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.util.Try

class IOSpec extends AnyFlatSpec with Matchers {

  private val isoDir = Paths.get("isometries")

  "LevelIO: readLevel()" should "read the levels/beginner/level1.txt file correctly" in {
    val path = "levels/beginner/level1.txt"

    val level = LevelIO.readLevel(path)

    level.length shouldBe 3
    level.foreach(_.length shouldBe 5)

    level(0) shouldBe Vector('-', '-', '-', '#', '-')
    level(1) shouldBe Vector('-', '-', '#', '-', '-')
    level(2) shouldBe Vector('-', '-', '-', '-', '-')
  }

  "MoveIO: readMoves()" should "parse moves src/test/resources/demo.txt file correctly" in {
    val path = "src/test/resources/demo.txt"

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

  "IsometryIO.save/load" should "persist and parse a composite iso without errors" in {
    val nameNoExt = "Unit_Iso_SaveLoad"
    val lines = Seq(
      "REFLECT_ROW;r1=0;c1=0;r2=1;c2=1;r0=2;merge=Opaque;boundary=Clipping",
      "REFLECT_COL;r1=3;c1=0;r2=4;c2=1;c0=2;merge=Opaque;boundary=Clipping",
      "REFLECT_ANTI;r1=3;c1=3;r2=4;c2=4;merge=Opaque;boundary=Clipping",
      "REFLECT_MAIN;r1=0;c1=0;r2=1;c2=1;merge=Opaque;boundary=Clipping",
      "ROTATE90;r1=0;c1=0;r2=1;c2=1;cr=2;cc=2;dir=CW;merge=Opaque;boundary=Clipping",
      "CENTRAL;r1=0;c1=3;r2=1;c2=4;cr=2;cc=2;merge=Opaque;boundary=Clipping",
      "TRANSLATE;r1=3;c1=0;r2=4;c2=1;dy=-1;dx=0;merge=Opaque;boundary=Clipping"
    )

    // save
    val saved = IsometryIO.save(nameNoExt, lines)
    saved.isRight shouldBe true

    // load (.txt)
    val loaded = IsometryIO.load(s"$nameNoExt.txt")
    loaded.isRight shouldBe true
    loaded.toOption.get shouldBe a[Iso]

    // cleanup
    deleteIfExists(isoDir.resolve(s"$nameNoExt.txt"))
  }
  
  "ScoreIO.append/readAll" should "round-trip multiple score rows in a fresh diff file" in {
    val diff = "unit_test_diff_intermediate"
    val scorePath = ScoresFs.fileFor(diff)
    
    deleteIfExists(scorePath)

    val rows = Vector(
      ScoreRow("Filip", 70, 42, 18, 1, "level1.txt"),
      ScoreRow("Ana", 83, 58, 25, 0, "level1.txt"),
      ScoreRow("Milan", 66, 46, 20, 0, "level2.txt")
    )

    rows.foreach(r => ScoreIO.append(diff, r))

    val readBack = ScoreIO.readAll(diff)
    readBack shouldBe rows

    // cleanup
    deleteIfExists(scorePath)
  }

  private def deleteIfExists(p: java.nio.file.Path): Unit = {
    Try(Files.deleteIfExists(p))
    ()
  }
}
