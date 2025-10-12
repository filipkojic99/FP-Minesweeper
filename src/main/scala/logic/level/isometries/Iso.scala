package logic.level.isometries

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

  protected inline def inBounds(lv: Level, r: Int, c: Int): Boolean =
    r >= 0 && r < lv.rows && c >= 0 && c < lv.cols

  protected inline def setAt(lv: Level, r: Int, c: Int, v: CellContent): Level = {
    val row2   = lv.cells(r).updated(c, v)
    val cells2 = lv.cells.updated(r, row2)
    lv.copy(cells = cells2)
  }

  /** 1) Snapshot: extract sector to Vector((mappedRC) -> value). */
  protected def snapshot(
                          level: Level,
                          n: Sector
                        )(mapRC: (Int, Int) => (Int, Int)): Vector[((Int, Int), CellContent)] =
    (n.r1 to n.r2).iterator.flatMap { r =>
      (n.c1 to n.c2).iterator.map { c =>
        val v      = level.cells(r)(c)
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

    val minR = img.map(_._1._1).min
    val maxR = img.map(_._1._1).max
    val minC = img.map(_._1._2).min
    val maxC = img.map(_._1._2).max

    val newTop  = math.min(0, minR)
    val newLeft = math.min(0, minC)
    val newRows = math.max(level.rows - newTop, maxR - newTop + 1)
    val newCols = math.max(level.cols - newLeft, maxC - newLeft + 1)

    if (newRows == level.rows && newCols == level.cols && newTop == 0 && newLeft == 0)
      (level, 0, 0)
    else {
      val blank  = Vector.fill(newRows, newCols)(CellContent.Clear)
      val copied =
        (0 until level.rows).foldLeft(Level(blank)) { (acc, r) =>
          (0 until level.cols).foldLeft(acc) { (acc2, c) =>
            setAt(acc2, r - newTop, c - newLeft, level.cells(r)(c))
          }
        }
      (copied, -newTop, -newLeft)
    }
  }

  /** 3) Clear: erase the original sector (with offsets). */
  protected def clearSector(base: Level, n: Sector, rOff: Int, cOff: Int): Level = {
    var acc = base
    var r   = n.r1
    while (r <= n.r2) {
      var c = n.c1
      while (c <= n.c2) {
        val rr = r + rOff
        val cc = c + cOff
        if (inBounds(acc, rr, cc)) acc = setAt(acc, rr, cc, CellContent.Clear)
        c += 1
      }
      r += 1
    }
    acc
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