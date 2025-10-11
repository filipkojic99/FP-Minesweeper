package logic.levelOps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import logic.level.{ColEdge, LevelOps, RowEdge}
import model.CellContent
import util.TestLevels

class LevelOpsSpec extends AnyFlatSpec with Matchers {
  "LevelOps: buildLevel()" should "read the src/test/resources/testIO.txt file correctly" in {
    val path = "src/test/resources/testIO.txt"

    val level = LevelOps.buildLevel(path)

    val expected = Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    )

    level.cells shouldBe expected

    level.rows shouldBe 2
    level.cols shouldBe 2
  }

  "LevelOps: addRow()" should "add a clear row at the top" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.addRow(level, RowEdge.Top, CellContent.Clear)

    result.rows shouldBe 6
    result.cols shouldBe 5
    result.cells.head.forall(_ == CellContent.Clear) shouldBe true
  }

  it should "add a clear row at the bottom" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.addRow(level, RowEdge.Bottom, CellContent.Clear)

    result.rows shouldBe 6
    result.cols shouldBe 5
    result.cells.last.forall(_ == CellContent.Clear) shouldBe true
  }

  "LevelOps: addCol()" should "add a clear column on the left" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.addCol(level, ColEdge.Left, CellContent.Clear)

    result.rows shouldBe 5
    result.cols shouldBe 6

    result.cells.forall(row => row.head == CellContent.Clear) shouldBe true
  }

  it should "add a clear column on the right" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.addCol(level, ColEdge.Right, CellContent.Clear)

    result.rows shouldBe 5
    result.cols shouldBe 6

    result.cells.forall(row => row.last == CellContent.Clear) shouldBe true
  }

  "LevelOps: removeRow()" should "remove the top row when there is more than one row" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.removeRow(level, RowEdge.Top)

    result.rows shouldBe 4
    result.cols shouldBe 5

    result.cells.head shouldBe level.cells(1)
  }

  it should "remove the bottom row when there is more than one row" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.removeRow(level, RowEdge.Bottom)

    result.rows shouldBe 4
    result.cols shouldBe 5

    result.cells.last shouldBe level.cells(level.rows - 2)
  }

  it should "do nothing if the level has only one row" in {
    val singleRowLevel = TestLevels.singleRowLevel()

    val resultTop = LevelOps.removeRow(singleRowLevel, RowEdge.Top)
    val resultBottom = LevelOps.removeRow(singleRowLevel, RowEdge.Bottom)

    resultTop shouldBe singleRowLevel
    resultBottom shouldBe singleRowLevel
  }

  "LevelOps: removeCol()" should "remove the leftmost column when there is more than one column" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.removeCol(level, ColEdge.Left)

    result.cols shouldBe 4
    result.rows shouldBe 5

    for (r <- result.cells.indices) {
      result.cells(r).head shouldBe level.cells(r)(1)
    }
  }

  it should "remove the rightmost column when there is more than one column" in {
    val level = TestLevels.sampleLevel()

    val result = LevelOps.removeCol(level, ColEdge.Right)

    result.cols shouldBe 4
    result.rows shouldBe 5

    for (r <- result.cells.indices) {
      result.cells(r).last shouldBe level.cells(r)(level.cols - 2)
    }
  }

  it should "do nothing if the level has only one column" in {
    val singleCol = TestLevels.singleColLevel()

    val resultLeft = LevelOps.removeCol(singleCol, ColEdge.Left)
    val resultRight = LevelOps.removeCol(singleCol, ColEdge.Right)

    resultLeft shouldBe singleCol
    resultRight shouldBe singleCol
  }

  "LevelOps: toggleAt()" should "toggle a mine to clear at the given position" in {
    val level = TestLevels.sampleLevel()
    val r = 0
    val c = 2

    val resultOpt = LevelOps.toggleAt(level, r, c)

    resultOpt.isDefined shouldBe true
    val result = resultOpt.get

    result.cells(r)(c) shouldBe CellContent.Clear

    result.cells(r).zipWithIndex.foreach {
      case (value, idx) if idx != c => value shouldBe level.cells(r)(idx)
      case _ =>
    }
  }

  it should "toggle a clear cell to a mine at the given position" in {
    val level = TestLevels.sampleLevel()
    val r = 1
    val c = 1

    val resultOpt = LevelOps.toggleAt(level, r, c)

    resultOpt.isDefined shouldBe true
    val result = resultOpt.get

    result.cells(r)(c) shouldBe CellContent.Mine

    result.cells(r).zipWithIndex.foreach {
      case (value, idx) if idx != c => value shouldBe level.cells(r)(idx)
      case _ =>
    }
  }

  it should "return None if coordinates are out of bounds" in {
    val level = TestLevels.sampleLevel()

    val result1 = LevelOps.toggleAt(level, -1, 0)
    val result2 = LevelOps.toggleAt(level, 0, 10)
    val result3 = LevelOps.toggleAt(level, 10, 10)

    result1 shouldBe None
    result2 shouldBe None
    result3 shouldBe None
  }

  "LevelOps: clearRect()" should "clear an inclusive in-bounds rectangle" in {
    val level = TestLevels.sampleLevel()
    val resOpt = LevelOps.clearRect(level, 0, 0, 1, 2)
    
    resOpt.isDefined shouldBe true
    val res = resOpt.get
    
    for (r <- 0 to 1; c <- 0 to 2) {
      res.cells(r)(c) shouldBe CellContent.Clear
    }
    
    // content outside of the bounds stays the same
    res.cells(2)(1) shouldBe level.cells(2)(1)
    res.cells(4)(4) shouldBe level.cells(4)(4)
  }

  it should "clamp coordinates that go out of bounds and still clear the overlapped area" in {
    val level = TestLevels.sampleLevel()
    val resOpt = LevelOps.clearRect(level, -10, -10, 2, 10)

    resOpt.isDefined shouldBe true
    val res = resOpt.get

    // clears rows from 0 to 2
    for (r <- 0 to 2; c <- 0 to 4) {
      res.cells(r)(c) shouldBe CellContent.Clear
    }

    // content outside of the bounds stays the same
    res.cells(4)(0) shouldBe level.cells(4)(0)
  }

  it should "return None if the rectangle is completely out of bounds (negative side)" in {
    val level = TestLevels.sampleLevel()
    val res = LevelOps.clearRect(level, -10, -10, -1, -1)
    res shouldBe None
  }

  it should "return None if the rectangle is completely out of bounds (beyond max)" in {
    val level = TestLevels.sampleLevel()
    val res = LevelOps.clearRect(level, 10, 10, 12, 12)
    res shouldBe None
  }
}
