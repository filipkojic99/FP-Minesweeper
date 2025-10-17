package logic.isometries

import logic.level.isometries.LevelIsometries.{reflectCol, reflectDiagAnti, reflectDiagMain, reflectRow}
import logic.level.isometries.{BoundaryMode, MergeMode, Sector}
import model.{CellContent, Level}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReflectSpec extends AnyFlatSpec with Matchers {

  // =========================================================
  behavior of "Transparent + Expanding (Row / Col / Main / Anti)"
  // =========================================================

  // ROW — Expanding UP: r' can become -1 -> addTop
  it should "Transparent+Expanding: reflect ROW(0) (expand up)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1), // sector 2*2
      rowIndex = 0,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear), // new mine (0,0)
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear) // (2,0) deleted because it is not covered
    ))

    result.rows shouldBe 3
    result.cols shouldBe 2
    result shouldBe expected
  }

  // COL — Expanding LEFT: c' can become -1 -> addLeft
  it should "Transparent+Expanding: reflect COL(0) (expand left)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 0,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear), // new mine (0,0)
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear) // (0,2) deleted
    ))

    result.rows shouldBe 2
    result.cols shouldBe 3
    result shouldBe expected
  }

  // MAIN diagonal reflection
  it should "Transparent+Expanding: reflect MAIN diagonal (no actual expansion)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectDiagMain(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine), // new mine (0,1)
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    result shouldBe expected
  }

  // ANTI diagonal reflection
  it should "Transparent+Expanding: reflect ANTI diagonal (no actual expansion)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagAnti(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine) // new mine (1,1)
    ))

    result shouldBe expected
  }


  // =========================================================
  behavior of "Opaque + Expanding (Row / Col / Main / Anti)"
  // =========================================================

  it should "Opaque+Expanding: reflect ROW(0) (expand up, overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1),
      rowIndex = 0,
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    result.rows shouldBe 3
    result.cols shouldBe 2
    result shouldBe expected
  }

  it should "Opaque+Expanding: reflect ROW(1) (expand down, overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1),
      rowIndex = 1,
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

  it should "Opaque+Expanding: reflect COL(0) (expand left, overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 0,
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    result.rows shouldBe 2
    result.cols shouldBe 3
    result shouldBe expected
  }

  it should "Opaque+Expanding: reflect COL(1) (expand right, overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 1,
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

  // MAIN diagonal
  it should "Opaque+Expanding: reflect MAIN diagonal (overwrite; no expansion)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectDiagMain(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    result shouldBe expected
  }

  // ANTI diagonal
  it should "Opaque+Expanding: reflect ANTI diagonal (overwrite; no expansion)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagAnti(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Expanding
    )

    val result = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine)
    ))

    result shouldBe expected
  }


  // =========================================================
  behavior of "Transparent + Clipping (Row / Col / Main / Anti)"
  // =========================================================

  it should "Transparent+Clipping: reflect ROW(0) (clip up → nothing added)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1),
      rowIndex = 0,
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

  it should "Transparent+Clipping: reflect COL(0) (clip left → nothing added)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 0,
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

  it should "Transparent+Clipping: reflect ROW(1) on 3x2 with 2x2 sector (in-bounds down)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1), // upper 2x2 sector
      rowIndex = 1,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear) // new mine (2,0)
    ))
    result shouldBe expected
  }

  it should "Transparent+Clipping: reflect COL(1) on 2x3 with 2x2 sector (in-bounds right)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1), // left 2x2 window
      colIndex = 1,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine) // new mine(1,2)
    ))
    result shouldBe expected
  }

  it should "Transparent+Clipping: reflect MAIN diagonal in 3x3 using 2x2 sector" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagMain(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "Transparent+Clipping: reflect ANTI diagonal in 3x3 using 2x2 sector" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagAnti(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }


  // =========================================================
  behavior of "Opaque + Clipping (Row / Col / Main / Anti)"
  // =========================================================

  it should "Opaque+Clipping: reflect ROW(0) (clip up → overwrite with clears)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1),
      rowIndex = 0,
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

  it should "Opaque+Clipping: reflect COL(0) (clip left → overwrite with clears)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 0,
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

  it should "Opaque+Clipping: reflect ROW(1) on 3x2 with 2x2 sector (in-bounds down)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectRow(
      sector = Sector(0, 0, 1, 1),
      rowIndex = 1,
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

  it should "Opaque+Clipping: reflect COL(1) on 2x3 with 2x2 sector (in-bounds right)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectCol(
      sector = Sector(0, 0, 1, 1),
      colIndex = 1,
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine)
    ))
    result shouldBe expected
  }

  it should "Opaque+Clipping: reflect MAIN diagonal in 3x3 using 2x2 sector (overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagMain(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }

  it should "Opaque+Clipping: reflect ANTI diagonal in 3x3 using 2x2 sector (overwrite)" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val iso = reflectDiagAnti(
      sector = Sector(0, 0, 1, 1),
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val result = iso(level)
    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    result shouldBe expected
  }
}
