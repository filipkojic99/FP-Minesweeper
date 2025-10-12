package logic.level.isometries

import model.CellContent

/** Rectangle sector in the grid. */
case class Sector(r1: Int, c1: Int, r2: Int, c2: Int) {

  def normalized: Sector = {
    val top = math.min(r1, r2)
    val left = math.min(c1, c2)
    val bottom = math.max(r1, r2)
    val right = math.max(c1, c2)
    Sector(top, left, bottom, right)
  }

  def rows: Int = math.abs(r2 - r1) + 1

  def cols: Int = math.abs(c2 - c1) + 1

  def contains(r: Int, c: Int): Boolean = {
    val s = normalized
    r >= s.r1 && r <= s.r2 && c >= s.c1 && c <= s.c2
  }
}

/** Reflexion axis. */
sealed trait Axis

object Axis {
  case class Row(r: Int) extends Axis

  case class Col(c: Int) extends Axis

  enum DiagonalKind {
    case Main, Anti
  }

  case class Diagonal(kind: DiagonalKind) extends Axis
}

/** Rotation direction. */
enum RotationDir {
  case CW, CCW
}

/** Merge image with original. */
sealed trait MergeMode {
  def merge(oldV: CellContent, imgV: CellContent): CellContent
}

object MergeMode {
  /** Transparent - superposition (OR Mine). */
  case object Transparent extends MergeMode {
    def merge(oldV: CellContent, imgV: CellContent): CellContent =
      if (oldV == CellContent.Mine || imgV == CellContent.Mine) CellContent.Mine
      else CellContent.Clear
  }

  /** Non - transparent - (overwrite Mine). */
  case object Opaque extends MergeMode {
    def merge(oldV: CellContent, imgV: CellContent): CellContent = imgV
  }
}

/** Handling image fields outside of the bounds. */
sealed trait BoundaryMode

object BoundaryMode {
  /** Expandable - expand the map to fit the image. */
  case object Expanding extends BoundaryMode

  /** Non - expandable - ignore fields outside the bounds. */
  case object Clipping extends BoundaryMode
}