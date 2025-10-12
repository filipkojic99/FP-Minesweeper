package logic.level.isometries.transformations

import logic.level.isometries.{Axis, BoundaryMode, Iso, IsoHelpers, MergeMode, Sector}
import model.Level

final case class Reflect(
                          sector: Sector,
                          axis: Axis,
                          merge: MergeMode,
                          boundary: BoundaryMode
                        ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val n = sector.normalized
    val img = snapshot(level, n)((r, c) => mapRC(n, r, c))
    val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
    val cleared = clearSector(base, n, rOff, cOff)
    mergeImage(cleared, img, rOff, cOff, merge)
  }

  def inverse: Iso = this // reflection is self-inverse

  /** Map (r,c) → (r',c') according to the selected axis. */
  private def mapRC(n: Sector, r: Int, c: Int): (Int, Int) = axis match {
    case Axis.Row(r0) => (2 * r0 - r, c)
    
    case Axis.Col(c0) => (r, 2 * c0 - c)

    case Axis.Diagonal(Axis.DiagonalKind.Main) =>
      // transpose within sector bounds
      val u = r - n.r1
      val v = c - n.c1
      (n.r1 + v, n.c1 + u)

    case Axis.Diagonal(Axis.DiagonalKind.Anti) =>
      // reflect across anti-diagonal within the sector
      val h = n.rows
      val w = n.cols
      val u = r - n.r1
      val v = c - n.c1
      (n.r1 + (w - 1 - v), n.c1 + (h - 1 - u))
  }
}
