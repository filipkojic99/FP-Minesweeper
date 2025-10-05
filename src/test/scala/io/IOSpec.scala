package io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IOSpec extends AnyFlatSpec with Matchers {

  "LevelIO: readLevel()" should "read the levels/beginner/level1.txt file correctly" in {
    val path = "levels/beginner/level1.txt"

    val level = LevelIO.readLevel(path)

    level.length shouldBe 3
    level.foreach(_.length shouldBe 5)

    level(0) shouldBe Vector('-', '-', '-', '#', '-')
    level(1) shouldBe Vector('-', '-', '#', '#', '-')
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
}
