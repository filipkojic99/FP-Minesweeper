package logic.level.isometries.transformations

import logic.level.isometries.*
import model.Level

case class Translate(
                            sector: Sector,
                            dy: Int, // +down, -up
                            dx: Int, // +right, -left
                            merge: MergeMode,
                            boundary: BoundaryMode
                          ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val n = sector.normalized
    val img = snapshot(level, n) { (r, c) => (r + dy, c + dx) }
    val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
    val cleared = clearSector(base, n, rOff, cOff)
    mergeImage(cleared, img, rOff, cOff, merge)
  }

  def inverse: Iso = copy(dy = -dy, dx = -dx)
}
