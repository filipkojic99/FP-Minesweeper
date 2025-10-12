package model

case class Level(cells: Vector[Vector[CellContent]]) {
  def rows: Int = cells.length

  def cols: Int = cells.head.length
}