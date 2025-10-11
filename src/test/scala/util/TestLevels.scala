package util

import model.{Level, CellContent}

object TestLevels {

  /** Returns a 5x5 test level with a fixed pattern of mines and clears. */
  def sampleLevel(): Level = {
    Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Mine)
    ))
  }

  /** Returns a 5x5 valid test level with a fixed pattern of mines and clears. */
  def validLevel(): Level = {
    Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Mine, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Clear)
    ))
  }

  /** Returns a single row test level with a fixed pattern of mines and clears. */
  def singleRowLevel(): Level = {
    Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Clear)
    ))
  }

  /** Returns a single column test level with a fixed pattern of mines and clears. */
  def singleColLevel(): Level = {
    Level(Vector(
      Vector(CellContent.Clear),
      Vector(CellContent.Mine),
      Vector(CellContent.Clear),
      Vector(CellContent.Mine),
      Vector(CellContent.Clear)
    ))
  }

  /** Returns non-rectangular test level with a fixed pattern of mines and clears. */
  def nonRectangularLevel(): Level = {
    Level(Vector(
      Vector(CellContent.Clear, CellContent.Clear, CellContent.Mine, CellContent.Clear, CellContent.Clear),
      Vector(CellContent.Mine, CellContent.Clear, CellContent.Clear, CellContent.Clear, CellContent.Mine),
      Vector(CellContent.Clear, CellContent.Mine, CellContent.Clear)
    ))
  }

  /** Beginner: 10x10 = 100 cells, allowed mines [10..16]. */
  def beginnerLevel(mines: Int = 12): Level =
    withKmines(rows = 10, cols = 10, k = mines)

  /** Intermediate: 10x15 = 150 cells, allowed mines [18..30]. */
  def intermediateLevel(mines: Int = 20): Level =
    withKmines(rows = 10, cols = 15, k = mines)

  /** Expert: 20x20 = 400 cells, allowed mines [64..100]. */
  def expertLevel(mines: Int = 80): Level =
    withKmines(rows = 20, cols = 20, k = mines)

  /** OutsideRanges: > 450 cells (npr. 21x22 = 462). All Clear. */
  def outsideRangeLevel(): Level =
    Level(Vector.fill(21, 22)(CellContent.Clear))

  /** Make level with k mines. */
  private def withKmines(rows: Int, cols: Int, k: Int): Level = {
    val total = rows * cols
    require(k >= 0 && k <= total, s"Invalid mine count k=$k for $rows x $cols")
    val cells = Vector.tabulate(rows, cols) { (r, c) =>
      val idx = r * cols + c
      if (idx < k) CellContent.Mine else CellContent.Clear
    }
    Level(cells)
  }
}
