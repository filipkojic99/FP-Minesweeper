package logic.level.isometries.transformations

import logic.level.isometries.{BoundaryMode, Iso, IsoHelpers, MergeMode, RotationDir, Sector}
import model.{CellContent, Level}

case class Rotate90(
                     sector: Sector,
                     center: (Int, Int),
                     dir: RotationDir,
                     merge: MergeMode,
                     boundary: BoundaryMode,
                     useImageSector: Boolean = false
                   ) extends Iso with IsoHelpers {

  def apply(level: Level): Level = {
    // 1) Odredi efektivni sektor (S ili S′ ako je kvazi-inverz)
    val effSector: Sector =
      if (!useImageSector) sector.normalized
      else {
        // forward smer je SUPROTAN od trenutnog, jer kvazi-inverz treba S′ dobijen originalnom transformacijom
        val forwardDir = dir match {
          case RotationDir.CW => RotationDir.CCW
          case RotationDir.CCW => RotationDir.CW
        }

        def forwardMap(r: Int, c: Int): (Int, Int) = {
          val (cr, cc) = center
          forwardDir match {
            case RotationDir.CW => (cr + (c - cc), cc - (r - cr))
            case RotationDir.CCW => (cr - (c - cc), cc + (r - cr))
          }
        }

        mapSectorVisible(level, sector, boundary)(forwardMap) match {
          case Some(sImg) => sImg
          case None => return level // nema vidljive slike → kvazi-inverz je no-op
        }
      }

    // 2) Sada standardna pipeline nad effSector, sa TRENUTNIM 'dir'
    def mapRC(r: Int, c: Int): (Int, Int) = {
      val (cr, cc) = center
      dir match {
        case RotationDir.CW => (cr + (c - cc), cc - (r - cr))
        case RotationDir.CCW => (cr - (c - cc), cc + (r - cr))
      }
    }

    val img = snapshot(level, effSector)(mapRC)
    val (base, rOff, cOff) = expandIfNeeded(level, img, boundary)
    val merged = mergeImage(base, img, rOff, cOff, merge)
    clearOutsideImage(merged, effSector, rOff, cOff, img)
  }

  def inverse: Iso = copy(
    dir = dir match {
    case RotationDir.CW => RotationDir.CCW
    case RotationDir.CCW => RotationDir.CW
  },
    useImageSector = true
  )

  override def precheck(level: Level): List[String] = {
    val s = sector.normalized
    val errs = List.newBuilder[String]
    if (boundary == BoundaryMode.Clipping) {
      if (s.r1 < 0 || s.c1 < 0 || s.r2 >= level.rows || s.c2 >= level.cols)
        errs += s"Sector out of bounds: [$s] vs level ${level.rows}x${level.cols}"
      val (cr, cc) = center
      if (cr < 0 || cr >= level.rows || cc < 0 || cc >= level.cols)
        errs += s"Center out of bounds: ($cr,$cc)"
    }
    errs.result()
  }

  /** Map (r,c) → (r',c') according to the selected direction. */
  private def mapRC(r: Int, c: Int): (Int, Int) = {
    val (cr, cc) = center
    dir match {
      case RotationDir.CW => (cr + (c - cc), cc - (r - cr))
      case RotationDir.CCW => (cr - (c - cc), cc + (r - cr))
    }
  }
}
