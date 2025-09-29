package logic

import model._

object BoardOps {

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

  private def fillAdjacencies(grid: Vector[Vector[Cell]]): Vector[Vector[Cell]] = {
    val b = Board(grid)
    grid.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (cell, c) =>
        cell.content match {
          case CellContent.Mine  => cell
          case CellContent.Clear =>
            val count = neighborsOf(b, Coord(r, c))
              .count(p => grid(p.row)(p.col).content == CellContent.Mine)
            cell.copy(adjacentMines = count)
        }
      }
    }
  }

  private def neighborsOf(b: Board, at: Coord): Vector[Coord] = {
    val deltas = Vector(
      (-1,-1), (-1,0), (-1,1),
      ( 0,-1),         ( 0,1),
      ( 1,-1), ( 1,0), ( 1,1)
    )
    deltas.flatMap { case (dr, dc) =>
      val r = at.row + dr
      val c = at.col + dc
      if (r >= 0 && r < b.rows && c >= 0 && c < b.cols) Some(Coord(r, c)) else None
    }
  }
}
