package logic.level.isometries

import logic.level.*
import logic.level.isometries.transformations.{CentralSymmetry, Reflect, Rotate90, Translate}

object LevelIsometries {
  def rotateCW(
                sector: Sector,
                center: (Int, Int),
                merge: MergeMode = MergeMode.Opaque,
                boundary: BoundaryMode = BoundaryMode.Clipping
              ): Iso = Rotate90(sector, center, RotationDir.CW, merge, boundary)

  def rotateCCW(
                 sector: Sector,
                 center: (Int, Int),
                 merge: MergeMode = MergeMode.Opaque,
                 boundary: BoundaryMode = BoundaryMode.Clipping
               ): Iso = Rotate90(sector, center, RotationDir.CCW, merge, boundary)

  def reflectRow(
                  sector: Sector, r0: Int,
                  merge: MergeMode = MergeMode.Opaque,
                  boundary: BoundaryMode = BoundaryMode.Clipping
                ): Iso = Reflect(sector, Axis.Row(r0), merge, boundary)

  def reflectCol(
                  sector: Sector, c0: Int,
                  merge: MergeMode = MergeMode.Opaque,
                  boundary: BoundaryMode = BoundaryMode.Clipping
                ): Iso = Reflect(sector, Axis.Col(c0), merge, boundary)

  def reflectDiagMain(
                       sector: Sector,
                       merge: MergeMode = MergeMode.Opaque,
                       boundary: BoundaryMode = BoundaryMode.Clipping
                     ): Iso = Reflect(sector, Axis.Diagonal(Axis.DiagonalKind.Main), merge, boundary)

  def reflectDiagAnti(
                       sector: Sector,
                       merge: MergeMode = MergeMode.Opaque,
                       boundary: BoundaryMode = BoundaryMode.Clipping
                     ): Iso = Reflect(sector, Axis.Diagonal(Axis.DiagonalKind.Anti), merge, boundary)

  def translate(
                 sector: Sector, dy: Int, dx: Int,
                 merge: MergeMode = MergeMode.Opaque,
                 boundary: BoundaryMode = BoundaryMode.Clipping
               ): Iso = Translate(sector, dy, dx, merge, boundary)

  def central(
               sector: Sector, center: (Int, Int),
               merge: MergeMode = MergeMode.Opaque,
               boundary: BoundaryMode = BoundaryMode.Clipping
             ): Iso = CentralSymmetry(sector, center, merge, boundary)

  /** Compose multiple isometries. */
  def composeAll(isos: List[Iso]): Iso =
    isos.foldLeft(Iso.Id: Iso) { (acc, iso) => acc.andThen(iso) }
}
