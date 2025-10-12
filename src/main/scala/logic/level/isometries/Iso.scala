package logic.level.isometries

import logic.level.{ColEdge, LevelOps, RowEdge}
import model.Level

trait Iso {
  /** Apply isometry on level. */
  def apply(level: Level): Level

  /** Undo applied isometry. */
  def inverse: Iso

  /** Compose isometries. */
  final def andThen(that: Iso): Iso = Iso.Composite(List(this, that))
}

trait IsoHelpers {

  import model.{CellContent, Level}
  import logic.level.isometries.{BoundaryMode, MergeMode, Sector}

  protected def inBounds(lv: Level, r: Int, c: Int): Boolean =
    r >= 0 && r < lv.rows && c >= 0 && c < lv.cols

  protected def setAt(lv: Level, r: Int, c: Int, v: CellContent): Level = {
    val row2 = lv.cells(r).updated(c, v)
    val cells2 = lv.cells.updated(r, row2)
    lv.copy(cells = cells2)
  }

  /** 1) Snapshot: extract sector to Vector((mappedRC) -> original value). */
  protected def snapshot(
                          level: Level,
                          n: Sector
                        )(mapRC: (Int, Int) => (Int, Int)): Vector[((Int, Int), CellContent)] =
    (n.r1 to n.r2).iterator.flatMap { r =>
      (n.c1 to n.c2).iterator.map { c =>
        val v = level.cells(r)(c)
        val (rp, cp) = mapRC(r, c)
        ((rp, cp), v)
      }
    }.toVector

  /** 2) Boundary: enlarge if needed, return (level, rowOffset, colOffset). */
  protected def expandIfNeeded(
                                level: Level,
                                img: Vector[((Int, Int), CellContent)],
                                boundary: BoundaryMode
                              ): (Level, Int, Int) = {
    if (boundary != BoundaryMode.Expanding || img.isEmpty) return (level, 0, 0)

    // find min and max row in expandable grid
    val minR = img.map(_._1._1).min
    val maxR = img.map(_._1._1).max
    val minC = img.map(_._1._2).min
    val maxC = img.map(_._1._2).max

    // add new rows and cols
    val addTop = math.max(0, -minR)
    val addLeft = math.max(0, -minC)
    val addBottom = math.max(0, maxR - (level.rows - 1))
    val addRight = math.max(0, maxC - (level.cols - 1))

    if (addTop == 0 && addLeft == 0 && addBottom == 0 && addRight == 0)
      return (level, 0, 0)

    val withTop = (0 until addTop).foldLeft(level)((lv, _) => LevelOps.addRow(lv, RowEdge.Top))
    val withBottom = (0 until addBottom).foldLeft(withTop)((lv, _) => LevelOps.addRow(lv, RowEdge.Bottom))
    val withLeft = (0 until addLeft).foldLeft(withBottom)((lv, _) => LevelOps.addCol(lv, ColEdge.Left))
    val expanded = (0 until addRight).foldLeft(withLeft)((lv, _) => LevelOps.addCol(lv, ColEdge.Right))

    (expanded, addTop, addLeft)
  }

  /** 3) Clear: erase the original sector (with offsets). */
  protected def clearSector(base: Level, n: Sector, rOff: Int, cOff: Int): Level = {
    val r1 = n.r1 + rOff
    val c1 = n.c1 + cOff
    val r2 = n.r2 + rOff
    val c2 = n.c2 + cOff
    LevelOps.clearRect(base, r1, c1, r2, c2).getOrElse(base)
  }

  /** 4) Merge: insert the transformed image according to MergeMode. */
  protected def mergeImage(
                            cleared: Level,
                            img: Vector[((Int, Int), CellContent)],
                            rOff: Int,
                            cOff: Int,
                            merge: MergeMode
                          ): Level =
    img.foldLeft(cleared) { case (acc, ((r, c), v)) =>
      val rr = r + rOff
      val cc = c + cOff
      if (!inBounds(acc, rr, cc)) acc
      else setAt(acc, rr, cc, merge.merge(acc.cells(rr)(cc), v))
    }
}

object Iso {
  /** Identity (no-op). */
  case object Id extends Iso {
    def apply(level: Level): Level = level

    def inverse: Iso = this
  }

  /** Composite isometry - pipeline. */
  case class Composite(steps: List[Iso]) extends Iso {
    def apply(level: Level): Level =
      steps.foldLeft(level)((acc, iso) => iso(acc))

    def inverse: Iso =
      Composite(steps.reverse.map(_.inverse))
  }
}