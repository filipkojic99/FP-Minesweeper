package logic.isometries

import logic.level.isometries.transformations.{CentralSymmetry, Rotate90, Translate}
import logic.level.isometries.{BoundaryMode, MergeMode, RotationDir, Sector}
import model.{CellContent, Level}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComposeIsoSpec extends AnyFlatSpec with Matchers {

  it should "Translate Transparent+Expanding: move 1x1 mine up-left and clear old sector cell" in {
    val level = Level(Vector(
      Vector(CellContent.Mine)
    ))

    val iso = Translate(
      sector = Sector(0, 0, 0, 0),
      dy = -1, dx = -1,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Expanding
    )

    val res = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    res shouldBe expected
  }

  it should "Translate Transparent+Clipping: move sector up out-of-bounds; clipped and sector cleared" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    val iso = Translate(
      sector = Sector(0, 0, 0, 0),
      dy = -1, dx = 0,
      merge = MergeMode.Transparent,
      boundary = BoundaryMode.Clipping
    )
    val res = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear)
    ))
    res shouldBe expected
  }

  it should "Translate Opaque+Clipping: shift right inside bounds and overwrite sector" in {
    val level = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear), // 2x3
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    val iso = Translate(
      sector = Sector(0, 0, 0, 1),
      dy = 0, dx = +1,
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )
    val res = iso(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    res shouldBe expected
  }

  it should "CentralSymmetry equals two Rotate90(CW) and is its own inverse" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Mine), // 3x3
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val center = (1, 1)

    val cs = CentralSymmetry(
      sector = Sector(0, 0, 2, 2),
      center = center,
      merge = MergeMode.Opaque,
      boundary = BoundaryMode.Clipping
    )

    val r90 = Rotate90(Sector(0, 0, 2, 2), center, RotationDir.CW, MergeMode.Opaque, BoundaryMode.Clipping)
    val r180 = r90.andThen(r90)

    val csRes = cs(level)
    val r180Res = r180(level)

    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Mine)
    ))

    csRes shouldBe expected
    r180Res shouldBe expected
    (cs.andThen(cs))(level) shouldBe level
  }

  it should "compose Translate and Rotate90: different results depending on order (no expanding)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val sector = Sector(0, 0, 2, 2)
    val center = (1, 1)

    val rot = Rotate90(sector, center, RotationDir.CW, MergeMode.Opaque, BoundaryMode.Clipping)

    val tr = Translate(sector, dy = +1, dx = 0, merge = MergeMode.Opaque, boundary = BoundaryMode.Clipping)

    val rotThenTr = rot.andThen(tr)(level)
    val expected1 = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    rotThenTr shouldBe expected1

    val trThenRot = tr.andThen(rot)(level)
    val expected2 = Level(Vector(
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    trThenRot shouldBe expected2
  }

  it should "compose CentralSymmetry and Translate; inverse chain returns original (no clipping loss)" in {
    val level = Level(Vector(
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))

    val sector = Sector(0, 0, 2, 2)
    val center = (1, 1)

    val cs = CentralSymmetry(sector, center, MergeMode.Opaque, BoundaryMode.Clipping)
    val tr = Translate(sector, dy = -1, dx = -1, merge = MergeMode.Opaque, boundary = BoundaryMode.Clipping)

    val combo = cs.andThen(tr)
    val invCombo = tr.inverse.andThen(cs.inverse)

    val comboRes = combo(level)
    val expected = Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
    comboRes shouldBe expected

    val back = invCombo(comboRes)
    back shouldBe level
  }
}
