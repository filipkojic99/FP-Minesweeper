package logic.level.isometries.transformations

import logic.level.isometries.*
import model.Level

final case class CentralSymmetry(
                                  sector: Sector,
                                  center: (Int, Int),
                                  merge: MergeMode,
                                  boundary: BoundaryMode
                                ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val rotateOnce  = Rotate90(sector, center, RotationDir.CW, merge, boundary)
    rotateOnce.andThen(rotateOnce)(level) // 90° + 90° = 180°
  }

  def inverse: Iso = this
}
