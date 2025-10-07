package logic.level

import io.LevelIO
import model.*

enum RowEdge { case Top, Bottom }

enum ColEdge { case Left, Right }

object LevelOps {

  /** Build level as matrix. */
  def buildLevel(path: String): Level = {
    val chars = LevelIO.readLevel(path)
    val prelim = chars.map { row =>
      row.map {
        case '#' => CellContent.Mine
        case '-' => CellContent.Clear
      }
    }
    Level(prelim)
  }

  /** Add new row to beginning/end, if possible. */
  def addRow(level: Level, edge: RowEdge, fill: CellContent = CellContent.Clear): Level = {
    val cols = level.cells.headOption.map(_.length).getOrElse(0)
    val row = Vector.fill(cols)(fill)
    val cells2 = edge match {
      case RowEdge.Top => row +: level.cells
      case RowEdge.Bottom => level.cells :+ row
    }
    level.copy(cells = cells2)
  }

  /** Add new column to beginning/end, if possible. */
  def addCol(level: Level, edge: ColEdge, fill: CellContent = CellContent.Clear): Level = {
    val cells2 = edge match {
      case ColEdge.Left =>
        level.cells.map(row => fill +: row)
      case ColEdge.Right =>
        level.cells.map(row => row :+ fill)
    }
    level.copy(cells = cells2)
  }

  /** Remove a row from beginning or end, if possible. */
  def removeRow(level: Level, edge: RowEdge): Level = {
    if (level.rows <= 1) level
    else {
      val updated = edge match {
        case RowEdge.Top    => level.cells.tail
        case RowEdge.Bottom => level.cells.init
      }
      level.copy(cells = updated)
    }
  }

  /** Remove a column from beginning or end, if possible. */
  def removeCol(level: Level, edge: ColEdge): Level = {
    if (level.cols <= 1) level
    else {
      val updated = edge match {
        case ColEdge.Left  => level.cells.map(_.tail)
        case ColEdge.Right => level.cells.map(_.init)
      }
      level.copy(cells = updated)
    }
  }

  /** Toggle Mine <-> Clear at (r,c). */
  def toggleAt(level: Level, r: Int, c: Int): Option[Level] = {
    modifyAt(level, r, c) {
      case CellContent.Mine => CellContent.Clear
      case CellContent.Clear => CellContent.Mine
    }
  }

  /** Apply a function to the cell at (r,c). */
  private def modifyAt(level: Level, r: Int, c: Int)(f: CellContent => CellContent): Option[Level] = {
    val rows = level.rows
    val cols = level.cols
    if (r < 0 || r >= rows || c < 0 || c >= cols) None
    else {
      val row = level.cells(r)
      val row2 = row.updated(c, f(row(c)))
      val cells2 = level.cells.updated(r, row2)
      Some(level.copy(cells = cells2))
    }
  }

  /** Clear rectangle from mines (set all cells inside to Clear). */
  def clearRect(level: Level, r1: Int, c1: Int, r2: Int, c2: Int): Option[Level] =
    modifyRect(level, r1, c1, r2, c2)(_ => CellContent.Clear)

  /** Apply a function to every cell inside the rectangle (inclusive). */
  private def modifyRect(
                          level: Level,
                          r1: Int, c1: Int,
                          r2: Int, c2: Int
                        )(f: CellContent => CellContent): Option[Level] = {
    val rows = level.rows
    val cols = level.cols

    val top = math.min(r1, r2).max(0)
    val left = math.min(c1, c2).max(0)
    val bottom = math.max(r1, r2).min(rows - 1)
    val right = math.max(c1, c2).min(cols - 1)

    if (top > bottom || left > right) return None

    val cells2 =
      level.cells.zipWithIndex.map { case (row, r) =>
        if (r >= top && r <= bottom)
          row.zipWithIndex.map { case (v, c) =>
            if (c >= left && c <= right) f(v) else v
          }
        else row
      }

    Some(level.copy(cells = cells2))
  }
}
