package model

enum CellContent {
  case Mine, Clear
}

case class Cell(content: CellContent, adjacentMines: Int)

case class Board(grid: Vector[Vector[Cell]]) {
  val rows: Int = grid.length
  val cols: Int = if (rows == 0) 0 else grid.head.length
}
