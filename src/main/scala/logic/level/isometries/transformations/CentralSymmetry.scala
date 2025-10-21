package logic.level.isometries.transformations

import logic.level.isometries.*
import model.Level

final case class CentralSymmetry(
                                  sector: Sector,
                                  center: (Int, Int),
                                  merge: MergeMode,
                                  boundary: BoundaryMode,
                                  useImageSector: Boolean = false
                                ) extends Iso with IsoHelpers {

  private def map180(r: Int, c: Int): (Int, Int) = {
    val (cr, cc) = center;
    (2 * cr - r, 2 * cc - c)
  }

  def apply(level: Level): Level = {
    val s0 = sector.normalized
    val effOpt =
      if (!useImageSector) Some(s0)
      else mapSectorVisible(level, s0, boundary) { (r, c) => map180(r, c) }

    effOpt match {
      case None => level
      case Some(effSector) =>
        val img = snapshot(level, effSector) { (r, c) => map180(r, c) }
        val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
        val merged = mergeImage(base, img, rOff, cOff, merge)
        clearOutsideImage(merged, effSector, rOff, cOff, img)
    }
  }

  def inverse: Iso = copy(
    useImageSector = true
  )

  override def precheck(level: Level): List[String] =
    Rotate90(sector, center, RotationDir.CW, merge, boundary).precheck(level)
}
