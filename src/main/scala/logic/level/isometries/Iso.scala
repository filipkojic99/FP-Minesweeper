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