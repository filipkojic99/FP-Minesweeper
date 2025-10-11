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
}
