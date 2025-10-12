package logic.level.isometries.transformations

import logic.level.isometries.{BoundaryMode, Iso, IsoHelpers, MergeMode, RotationDir, Sector}
import model.{CellContent, Level}

case class Rotate90(
                     sector: Sector,
                     center: (Int, Int),
                     dir: RotationDir,
                     merge: MergeMode,
                     boundary: BoundaryMode
                   ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val n = sector.normalized
    val img = snapshot(level, n)(mapRC)
    val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
    val cleared = clearSector(base, n, rOff, cOff)
    mergeImage(cleared, img, rOff, cOff, merge)
  }

  def inverse: Iso = copy(dir = dir match {
    case RotationDir.CW => RotationDir.CCW
    case RotationDir.CCW => RotationDir.CW
  })

  private def mapRC(r: Int, c: Int): (Int, Int) = {
    val (cr, cc) = center
    dir match {
      case RotationDir.CW => (cr + (c - cc), cc - (r - cr))
      case RotationDir.CCW => (cr - (c - cc), cc + (r - cr))
    }
  }
}
