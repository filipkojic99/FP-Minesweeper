package logic.level.isometries.transformations

import logic.level.isometries.{Axis, BoundaryMode, Iso, IsoHelpers, MergeMode, Sector}
import model.Level

final case class Reflect(
                          sector: Sector,
                          axis: Axis,
                          merge: MergeMode,
                          boundary: BoundaryMode,
                          useImageSector: Boolean = false
                        ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    val s0 = sector.normalized

    // 1) Efektivni sektor (S ili S′) kao Option
    val effOpt: Option[Sector] =
      if (!useImageSector) Some(s0)
      else mapSectorVisible(level, s0, boundary) { (r, c) =>
        // refleksija je samoinverzna → forward mapa = ista mapa
        mapRC(level, s0, r, c)
      }

    // 2) Ako nema vidljive slike u Clipping režimu → vrati neizmenjen level
    effOpt match {
      case None => level
      case Some(effSector) =>
        val img = snapshot(level, effSector) { (r, c) => mapRC(level, effSector, r, c) }
        val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
        val merged = mergeImage(base, img, rOff, cOff, merge)
        clearOutsideImage(merged, effSector, rOff, cOff, img)
    }
  }

  def inverse: Iso = copy(
    useImageSector = true
  ) // reflection is self-inverse

  override def precheck(level: Level): List[String] = {
    val s = sector.normalized
    val errs = List.newBuilder[String]
    if (boundary == BoundaryMode.Clipping) {
      if (s.r1 < 0 || s.c1 < 0 || s.r2 >= level.rows || s.c2 >= level.cols)
        errs += s"Sector out of bounds: [$s]"
      axis match {
        case Axis.Row(r0) if r0 < 0 || r0 >= level.rows =>
          errs += s"Row axis out of bounds: r=$r0"
        case Axis.Col(c0) if c0 < 0 || c0 >= level.cols =>
          errs += s"Col axis out of bounds: c=$c0"
        case _ => ()
      }
    }
    errs.result()
  }

  /** Map (r,c) → (r',c') according to the selected axis. */
  private def mapRC(level: Level, n: Sector, r: Int, c: Int): (Int, Int) = axis match {
    case Axis.Row(r0) => (2 * r0 - r, c)
    
    case Axis.Col(c0) => (r, 2 * c0 - c)

    case Axis.Diagonal(Axis.DiagonalKind.Main) =>
      // transpose within sector bounds
//      val u = r - n.r1
//      val v = c - n.c1
//      (n.r1 + v, n.c1 + u)
      (c, r)

    case Axis.Diagonal(Axis.DiagonalKind.Anti) =>
      // reflect across anti-diagonal within the sector
//      val h = n.rows
//      val w = n.cols
//      val u = r - n.r1
//      val v = c - n.c1
//      (n.r1 + (w - 1 - v), n.c1 + (h - 1 - u))
      val rows = level.rows
      val cols = level.cols
      ((cols - 1) - c, (rows - 1) - r)
  }
}
