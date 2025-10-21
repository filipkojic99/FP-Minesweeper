package logic.isometries

import logic.level.isometries.LevelIsometries.rotateCW
import logic.level.isometries.LevelIsometries.rotateCCW
import logic.level.isometries.{BoundaryMode, MergeMode, Sector}
import model.{CellContent, Level}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Rotate90Spec extends AnyFlatSpec with Matchers {

  behavior of "rotate with Transparent + Expanding (four directions, CW/CCW, varying centers)"

  it should "expand UP with CW around (0,1) on the whole matrix" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))


    result shouldBe expected
  }

  it should "expand LEFT with CW around (0,0) on a sub-sector of a wider level" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 0),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))


    result shouldBe expected
  }

  it should "expand DOWN with CCW around (1,1) on a sub-sector of a taller level" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    result shouldBe expected
  }

  it should "expand RIGHT with CCW around (0,1) on the whole matrix" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine)
    ))

    result shouldBe expected
  }

  behavior of "rotate with Opaque + Expanding (four directions, CW/CCW, varying centers)"

  it should "expand UP with CW around (0,1) on the whole matrix (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    result.rows shouldBe 3
    result.cols shouldBe 2
    result shouldBe expected
  }

  it should "expand LEFT with CW around (0,0) on a sub-sector (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 0),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    result.rows shouldBe 2
    result.cols shouldBe 4
    result shouldBe expected
  }

  it should "expand DOWN with CW around (1,0) on a sub-sector (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (1, 0),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    result.rows shouldBe 3
    result.cols shouldBe 2
    result shouldBe expected
  }

  it should "expand RIGHT with CCW around (0,1) on the whole matrix (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)
    
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine)
    ))

    result.rows shouldBe 2
    result.cols shouldBe 3
    result shouldBe expected
  }

  behavior of "rotate with Transparent + Clipping (four directions, CW/CCW, varying centers)"

  it should "clip UP with CW around (0,1) on the whole matrix" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip LEFT with CW around (0,0) on a sub-sector of a wider level" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 0),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip DOWN with CCW around (1,1) on a sub-sector of a taller level" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip RIGHT with CCW around (0,1) on the whole matrix" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  behavior of "rotate with Opaque + Clipping (four directions, CW/CCW, varying centers)"

  it should "clip UP with CW around (0,1) on the whole matrix (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip LEFT with CW around (0,0) on a sub-sector of a wider level (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 0),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip DOWN with CCW around (1,1) on a sub-sector of a taller level (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (1, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "clip RIGHT with CCW around (0,1) on the whole matrix (opaque overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = rotateCCW(
      sector = Sector(0, 0, 1, 1),
      center = (0, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

}
