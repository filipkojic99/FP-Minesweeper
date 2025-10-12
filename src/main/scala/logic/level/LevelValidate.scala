package logic.level

import model.{Level, CellContent}

enum LevelDifficulty {
  case Beginner, Intermediate, Expert
}

/** Holds the computed validation data for a level. */
final case class ValidationInfo(
                                 difficulty: LevelDifficulty,
                                 cells: Int,
                                 mines: Int,
                                 minMines: Int,
                                 maxMines: Int
                               )


/** Base trait for all possible validation errors. */
sealed trait ValidationError

/** Level rows are not rectangular (different lengths). */
case object NotRectangular extends ValidationError

/** The number of mines is outside of allowed range for this difficulty. */
case object MinesOutOfRange extends ValidationError

/** The total board size is outside of supported limits. */
case object OutsideRanges extends ValidationError

object LevelValidate {

  /**
   * Validate a level and return either:
   *  - Left(List of validation errors), or
   *  - Right(ValidationInfo) if the level is valid.
   */
  def validate(level: Level): Either[List[ValidationError], ValidationInfo] = {
    val errs = List.newBuilder[ValidationError]

    // 1. Check rectangular shape (all rows must have equal length)
    if (!isRectangular(level)) errs += NotRectangular

    val rows = level.rows
    val cols = level.cols
    val cells = rows * cols
    val mines = mineCount(level)

    // 2. Determine difficulty based on number of cells
    val diffOpt =
      if (cells >= 1 && cells <= 100) Some(LevelDifficulty.Beginner)
      else if (cells >= 101 && cells <= 300) Some(LevelDifficulty.Intermediate)
      else if (cells >= 301 && cells <= 450) Some(LevelDifficulty.Expert)
      else None

    // If the board size doesn’t fit any difficulty range
    if (diffOpt.isEmpty) errs += OutsideRanges

    // 3. Compute allowed mine count boundaries for the detected difficulty
    val (minM, maxM) = diffOpt match {
      case Some(LevelDifficulty.Beginner) => bounds(cells, 0.10, 0.16)
      case Some(LevelDifficulty.Intermediate) => bounds(cells, 0.12, 0.20)
      case Some(LevelDifficulty.Expert) => bounds(cells, 0.16, 0.25)
      case None => (0, 0)
    }

    // 4. Validate that mine count fits into range
    if (diffOpt.nonEmpty && (mines < minM || mines > maxM)) errs += MinesOutOfRange

    // 5. Return accumulated errors or success info
    val es = errs.result()
    if (es.nonEmpty) Left(es)
    else Right(ValidationInfo(diffOpt.get, cells, mines, minM, maxM))
  }

  /** Returns true if all rows have the same number of columns. */
  private def isRectangular(level: Level): Boolean =
    level.cells.isEmpty || level.cells.map(_.length).distinct.size == 1

  /** Count all mines in the grid. */
  private def mineCount(level: Level): Int =
    level.cells.flatten.count(_ == CellContent.Mine)

  /**
   * Compute the minimum and maximum number of mines based on the percentage
   * for the given difficulty. Clamped so that there is at least one mine
   * and at least one clear cell.
   */
  private def bounds(cells: Int, pMin: Double, pMax: Double): (Int, Int) = {
    val rawMin = math.floor(pMin * cells).toInt
    val rawMax = math.ceil(pMax * cells).toInt
    val minM = math.max(1, rawMin)
    val maxM = math.min(cells - 1, math.max(minM, rawMax))
    (minM, maxM)
  }
}
