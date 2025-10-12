package logic.level.isometries

import model.Level

/** 90° rotation around center. */
case class Rotate90(
                     sector: Sector,
                     center: (Int, Int),
                     dir: RotationDir,
                     merge: MergeMode,
                     boundary: BoundaryMode
                   ) extends Iso {
  def apply(level: Level): Level = {
    // TODO:
    // 1) izdvoji sliku sektora
    // 2) za svaku (r,c) iz sektora izračunaj (r',c') oko center po dir (CW/CCW)
    // 3) ako boundary==Expanding: proširi level po potrebi; inače, klipuj spoljne tačke
    // 4) očisti originalni sektor
    // 5) spoji sliku sa originalom korišćenjem merge.merge(...)
    level
  }

  def inverse: Iso = copy(dir = dir match {
    case RotationDir.CW  => RotationDir.CCW
    case RotationDir.CCW => RotationDir.CW
  })
}

/** Reflextion based on axis. */
case class Reflect(
                    sector: Sector,
                    axis: Axis,
                    merge: MergeMode,
                    boundary: BoundaryMode
                  ) extends Iso {
  def apply(level: Level): Level = {
    // TODO:
    // 1) izdvoji sliku sektora
    // 2) preslikaj (r,c) → (r',c') zavisno od Axis:
    //    - Row(r0):     (r',c') = (2*r0 - r, c)
    //    - Col(c0):     (r',c') = (r, 2*c0 - c)
    //    - Diagonal.Main: (r',c') = (c, r)  (transponovanje)
    //    - Diagonal.Anti: (r',c') = ( (cols-1 - c)+r1, (rows-1 - r)+c1 ) prilagođeno sektoru
    // 3) boundary Expanding/Clipping
    // 4) očisti originalni sektor
    // 5) merge
    level
  }

  def inverse: Iso = this // refleksija je sopstveni inverz
}

/** Sector translation. */
case class Translate(
                      sector: Sector,
                      dy: Int, // +dole, -gore
                      dx: Int, // +desno, -levo
                      merge: MergeMode,
                      boundary: BoundaryMode
                    ) extends Iso {
  def apply(level: Level): Level = {
    // TODO:
    // 1) slika = sektor pomeren za (dy,dx)
    // 2) boundary
    // 3) očisti originalni sektor
    // 4) merge
    level
  }

  def inverse: Iso = copy(dy = -dy, dx = -dx)
}

/** Centralna simetrija (rotacija 180°), može kao kompozicija dve Rotate90. */
case class CentralSymmetry(
                            sector: Sector,
                            center: (Int, Int),
                            merge: MergeMode,
                            boundary: BoundaryMode
                          ) extends Iso {
  def apply(level: Level): Level = {
    // TODO: najjednostavnije: Rotate90(...CW) andThen Rotate90(...CW)
    level
  }
  def inverse: Iso = this
}