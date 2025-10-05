package logic

import model._

object BoardOps {

  /** Build board as matrix with number of adjacent mines for every field. */
  def buildFromChars(chars: Vector[Vector[Char]]): Board = {
    val prelim = chars.map { row =>
      row.map {
        case '#' => Cell(CellContent.Mine, 0)
        case '-' => Cell(CellContent.Clear, 0)
      }
    }
    val withAdj = fillAdjacencies(prelim)
    Board(withAdj)
  }

  def neighbors8(b: Board, r: Int, c: Int): Vector[(Int, Int)] =
    neighborsOf(b, r, c)

  /** Count number of adjacent mines for every field. */
  private def fillAdjacencies(grid: Vector[Vector[Cell]]): Vector[Vector[Cell]] = {
    val b = Board(grid)
    grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        cell.content match {
          case CellContent.Mine => cell
          case CellContent.Clear =>
            val count =
              neighborsOf(b, r, c).count { case (rr, cc) =>
                grid(rr)(cc).content == CellContent.Mine
              }
            cell.copy(adjacentMines = count)
        }
      }
    }
  }

  /** Return vector of neighbour coordinates. */
  private def neighborsOf(b: Board, r: Int, c: Int): Vector[(Int, Int)] = {
    val deltas = Vector(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    )
    deltas.flatMap { case (dr, dc) =>
      val rr = r + dr
      val cc = c + dc
      if (rr >= 0 && rr < b.rows && cc >= 0 && cc < b.cols) Some((rr, cc)) else None
    }
  }
}
