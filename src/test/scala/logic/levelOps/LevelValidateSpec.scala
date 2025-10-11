package logic.levelOps

import logic.level.{LevelDifficulty, LevelValidate, MinesOutOfRange, NotRectangular, OutsideRanges}
import model.{CellContent, Level}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.TestLevels

class LevelValidateSpec extends AnyFlatSpec with Matchers {
  "LevelValidate.validate()" should "return NotRectangular when rows have different lengths" in {
    val level = TestLevels.nonRectangularLevel()

    val res = LevelValidate.validate(level)

    res.isLeft shouldBe true
    res.left.get should contain(NotRectangular)
  }

  it should "NOT include NotRectangular for a proper rectangular level" in {
    val level = TestLevels.validLevel()

    val res = LevelValidate.validate(level)

    res.isLeft shouldBe false
    res.isRight shouldBe true
  }

   it should "classify 100 cells as Beginner (within bounds)" in {
    val level: Level = TestLevels.beginnerLevel()

    val res = LevelValidate.validate(level)

    res.isRight shouldBe true
    val info = res.right.get
    info.difficulty shouldBe LevelDifficulty.Beginner
    info.cells shouldBe 100
    info.mines shouldBe 12
    info.minMines shouldBe 10
    info.maxMines shouldBe 16
  }

  it should "classify 150 cells as Intermediate (within bounds)" in {
    val level: Level = TestLevels.intermediateLevel()

    val res = LevelValidate.validate(level)

    res.isRight shouldBe true
    val info = res.right.get
    info.difficulty shouldBe LevelDifficulty.Intermediate
    info.cells shouldBe 150
    info.mines shouldBe 20
    info.minMines shouldBe 18
    info.maxMines shouldBe 30
  }

  it should "classify 400 cells as Expert (within bounds)" in {
    val level: Level = TestLevels.expertLevel()

    val res = LevelValidate.validate(level)

    res.isRight shouldBe true
    val info = res.right.get
    info.difficulty shouldBe LevelDifficulty.Expert
    info.cells shouldBe 400
    info.mines shouldBe 80
    info.minMines shouldBe 64
    info.maxMines shouldBe 100
  }

  it should "return OutsideRanges when cells > 450" in {
    val level: Level = TestLevels.outsideRangeLevel() // 21x22 = 462

    val res = LevelValidate.validate(level)

    res.isLeft shouldBe true
    res.left.get should contain(OutsideRanges)
  }

  it should "accept Beginner exactly at min and max, and reject below/above" in {
    val atMin = TestLevels.beginnerLevel(mines = 10) // min
    val atMax = TestLevels.beginnerLevel(mines = 16) // max
    val below = TestLevels.beginnerLevel(mines = 9) // < min
    val above = TestLevels.beginnerLevel(mines = 17) // > max

    LevelValidate.validate(atMin).isRight shouldBe true
    LevelValidate.validate(atMax).isRight shouldBe true

    val rBelow = LevelValidate.validate(below)
    rBelow.isLeft shouldBe true
    rBelow.left.get should contain(MinesOutOfRange)

    val rAbove = LevelValidate.validate(above)
    rAbove.isLeft shouldBe true
    rAbove.left.get should contain(MinesOutOfRange)
  }

  it should "accept Intermediate exactly at min and max, and reject below/above" in {
    val atMin = TestLevels.intermediateLevel(mines = 18)
    val atMax = TestLevels.intermediateLevel(mines = 30)
    val below = TestLevels.intermediateLevel(mines = 17)
    val above = TestLevels.intermediateLevel(mines = 31)

    LevelValidate.validate(atMin).isRight shouldBe true
    LevelValidate.validate(atMax).isRight shouldBe true

    val rBelow = LevelValidate.validate(below)
    rBelow.isLeft shouldBe true
    rBelow.left.get should contain(MinesOutOfRange)

    val rAbove = LevelValidate.validate(above)
    rAbove.isLeft shouldBe true
    rAbove.left.get should contain(MinesOutOfRange)
  }

  it should "accept Expert exactly at min and max, and reject below/above" in {
    val atMin = TestLevels.expertLevel(mines = 64)
    val atMax = TestLevels.expertLevel(mines = 100)
    val below = TestLevels.expertLevel(mines = 63)
    val above = TestLevels.expertLevel(mines = 101)

    LevelValidate.validate(atMin).isRight shouldBe true
    LevelValidate.validate(atMax).isRight shouldBe true

    val rBelow = LevelValidate.validate(below)
    rBelow.isLeft shouldBe true
    rBelow.left.get should contain(MinesOutOfRange)

    val rAbove = LevelValidate.validate(above)
    rAbove.isLeft shouldBe true
    rAbove.left.get should contain(MinesOutOfRange)
  }

  it should "treat 1x1 as Beginner but with impossible bounds - MinesOutOfRange" in {
    val level = Level(Vector(Vector(CellContent.Clear)))

    val res = LevelValidate.validate(level)

    res.isLeft shouldBe true
    res.left.get should contain(MinesOutOfRange)
    res.left.get should not contain (OutsideRanges)
  }

  it should "for 2 cells allow only exactly 1 mine (0 -> invalid, 2 -> invalid, 1 -> valid)" in {
    val zeroMines = Level(Vector(Vector(CellContent.Clear, CellContent.Clear)))
    val oneMine = Level(Vector(Vector(CellContent.Mine, CellContent.Clear)))
    val twoMines = Level(Vector(Vector(CellContent.Mine, CellContent.Mine)))

    val r0 = LevelValidate.validate(zeroMines)
    r0.isLeft shouldBe true
    r0.left.get should contain(MinesOutOfRange)

    val r2 = LevelValidate.validate(twoMines)
    r2.isLeft shouldBe true
    r2.left.get should contain(MinesOutOfRange)

    val r1 = LevelValidate.validate(oneMine)
    r1.isRight shouldBe true
    val info = r1.right.get
    info.mines shouldBe 1
    info.minMines shouldBe 1
    info.maxMines shouldBe 1
  }
  
  "LevelValidate.validate()" should "return both NotRectangular and OutsideRanges when non-rectangular level has > 450 cells" in {
    val fullRows: Vector[Vector[CellContent]] = Vector.fill(20)(Vector.fill(22)(CellContent.Clear))
    val shortRow: Vector[CellContent] = Vector.fill(21)(CellContent.Clear)
    val level = Level(fullRows :+ shortRow)

    val res = LevelValidate.validate(level)

    res.isLeft shouldBe true
    val errs = res.left.get
    errs should contain(NotRectangular)
    errs should contain(OutsideRanges)
  }
  
  it should "accept a valid Beginner example (e.g., 5x5 with 3 mines) and return proper info" in {
    val level = TestLevels.validLevel()

    val res = LevelValidate.validate(level)

    res.isRight shouldBe true
    val info = res.right.get

    info.difficulty shouldBe LevelDifficulty.Beginner
    info.cells shouldBe 25
    info.mines shouldBe 3
    info.minMines shouldBe 2 // floor(0.10 * 25) = 2
    info.maxMines shouldBe 4 // ceil(0.16 * 25)  = 4
  }
}