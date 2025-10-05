package logic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.*

class BoardOpsSpec extends AnyFlatSpec with Matchers {

  "BoardOps: buildFromChars()" should "create a board with correct dimensions" in {
    val chars = Vector(
      Vector('-', '#', '-'),
      Vector('-', '-', '#'),
    )

    val board = BoardOps.buildFromChars(chars)

    board.rows shouldBe 2
    board.cols shouldBe 3
  }

  it should "create a board with correct types" in {
    val chars = Vector(
      Vector('-', '#', '-'),
      Vector('-', '-', '#'),
    )

    val board = BoardOps.buildFromChars(chars)

    val expected = Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine)
    )

    val actual = board.grid.map(row => {
      row.map {
        cell => cell.content
      }
    })

    actual shouldBe expected
  }

  it should "create a board with correctly computed adjacent mines for every field" in {
    val chars = Vector(
      Vector('-', '#', '-', '-', '#'),
      Vector('-', '-', '-', '#', '#'),
      Vector('-', '#', '-', '-', '#'),
      Vector('-', '-', '#', '-', '#'),
    )

    val board = BoardOps.buildFromChars(chars)

    val expected = Vector(
      Vector(1, 0, 2, 3, 0),
      Vector(2, 2, 3, 0, 0),
      Vector(1, 0, 3, 5, 0),
      Vector(1, 2, 0, 3, 0)
    )

    val actual = board.grid.map( row => {
      row.map {
        cell => cell.adjacentMines
      }
    })

    actual shouldBe expected
  }

  "BoardOps: neighbors8()" should "return correct neighbors for corner, edge and center" in {
    val b = BoardOps.buildFromChars(Vector(
      Vector('-', '-', '-'),
      Vector('-', '-', '-')
    ))
    BoardOps.neighbors8(b, 0, 0).toSet shouldBe Set((0, 1), (1, 0), (1, 1))
    BoardOps.neighbors8(b, 1, 2).toSet shouldBe Set((0, 1), (0, 2), (1, 1))
    BoardOps.neighbors8(b, 1, 1).toSet shouldBe Set((0, 0), (0, 1), (0, 2), (1, 0), (1, 2))
  }
}

