package logic.level.isometries.transformations

import logic.level.isometries.*
import model.Level

case class Translate(
                      sector: Sector,
                      dy: Int, // +down, -up
                      dx: Int, // +right, -left
                      merge: MergeMode,
                      boundary: BoundaryMode,
                      useImageSector: Boolean = false
                    ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val s0 = sector.normalized
    val effOpt =
      if (!useImageSector) Some(s0)
      else mapSectorVisible(level, s0, boundary) { (r, c) => (r - dy, c - dx) }

    effOpt match {
      case None => level
      case Some(effSector) =>
        val img = snapshot(level, effSector) { (r, c) => (r + dy, c + dx) }
        val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
        val merged = mergeImage(base, img, rOff, cOff, merge)
        clearOutsideImage(merged, effSector, rOff, cOff, img)
    }
  }

  def inverse: Iso = copy(
    dy = -dy,
    dx = -dx,
    useImageSector = true
  )

  override def precheck(level: Level): List[String] = {
    if (boundary == BoundaryMode.Expanding) Nil
    else {
      val s = sector.normalized
      val r1 = s.r1 + dy;
      val r2 = s.r2 + dy
      val c1 = s.c1 + dx;
      val c2 = s.c2 + dx
      val fullyOutside =
        r2 < 0 || r1 >= level.rows || c2 < 0 || c1 >= level.cols
      val errs = List.newBuilder[String]
      if (s.r1 < 0 || s.c1 < 0 || s.r2 >= level.rows || s.c2 >= level.cols)
        errs += s"Sector out of bounds: [$s]"
      if (fullyOutside)
        errs += "Translated image fully outside with Clipping (no effect)."
      errs.result()
    }
  }
}
