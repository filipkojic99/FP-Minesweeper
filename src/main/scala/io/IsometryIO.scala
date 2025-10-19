package io

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._
import scala.util.Try

import logic.level.isometries._
import logic.level.isometries.transformations._
import model.Level

object IsometryIO {

  private val Dir = Paths.get("isometries")
  private val NamePrefix = "# name:"

  private def ensureDir(): Unit =
    if (!Files.exists(Dir)) Files.createDirectories(Dir)

  /** Listing svih .txt fajlova iz isometries/ (npr. ["SpiralCW.txt", ...]) */
  def list(): Seq[String] = {
    ensureDir()
    Files.list(Dir).iterator().asScala
      .filter(Files.isRegularFile(_))
      .map(_.getFileName.toString)
      .filter(_.toLowerCase.endsWith(".txt"))
      .toSeq
      .sorted
  }

  /** Puna putanja za zadato "ime" bez ekstenzije. */
  def pathFor(nameNoExt: String): String = {
    ensureDir()
    Dir.resolve(s"${sanitize(nameNoExt)}.txt").toString
  }

  /** Snimi kompozitnu izometriju u svoj fajl.
   * `lines` su linije koraka (bez headera); header sa # name: se dodaje automatski.
   * Ako fajl postoji, prebrisaće se (možeš dodati flag ako želiš zaštitu).
   */
  def save(nameNoExt: String, lines: Seq[String]): Either[String, String] = Try {
    ensureDir()
    val p = Paths.get(pathFor(nameNoExt))
    val header = s"$NamePrefix ${nameNoExt}\n"
    val content = (header + lines.mkString("\n") + "\n").getBytes("UTF-8")
    Files.write(
      p,
      content,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    p.toString
  }.toEither.left.map(_.getMessage)

  /** Učitaj kompozitnu izometriju iz fajla (fileName sa .txt). */
  def load(fileName: String): Either[String, Iso] = Try {
    ensureDir()
    val p = Dir.resolve(fileName)
    val raw = Files.readAllLines(p).asScala.toVector
    val stepLines = raw.iterator
      .map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("#"))
      .toVector

    val steps: List[Iso] = stepLines.map(parseLine).toList
    Iso.Composite(steps)
  }.toEither.left.map(_.getMessage)

  /** Obriši fajl izometrije. */
  def delete(fileName: String): Either[String, Unit] = Try {
    ensureDir()
    val p = Dir.resolve(fileName)
    Files.deleteIfExists(p)
    ()
  }.toEither.left.map(_.getMessage)

  /** Lepo ime za prikaz: čita # name: ako postoji, inače koristi naziv fajla bez .txt. */
  def displayLabel(fileName: String): String = {
    readName(fileName).getOrElse(fileName.stripSuffix(".txt"))
  }

  /** Čita # name: sa prve linije, ako postoji. */
  def readName(fileName: String): Option[String] = {
    ensureDir()
    val p = Dir.resolve(fileName)
    if (!Files.exists(p)) return None
    val it = Files.lines(p).iterator()
    try {
      if (it.hasNext) {
        val first = it.next().trim
        if (first.startsWith(NamePrefix)) Some(first.stripPrefix(NamePrefix).trim)
        else None
      } else None
    } finally {
      // Java stream se zatvara preko Files.lines(p) autoclose, pa je ok
    }
  }

  // ----------------- Parsiranje jedne linije u Iso -----------------

  private def parseLine(line: String): Iso = {
    val parts = line.split(";").map(_.trim).filter(_.nonEmpty)
    if (parts.isEmpty) sys.error("Empty isometry step line.")
    val kind = parts.head
    val kv = parts.tail.map { token =>
      val i = token.indexOf('=')
      if (i <= 0) sys.error(s"Bad token: $token")
      token.substring(0, i) -> token.substring(i + 1)
    }.toMap

    def g(k: String): String = kv.getOrElse(k, sys.error(s"Missing param: $k"))
    def gi(k: String): Int  = g(k).toInt

    val sector = Sector(gi("r1"), gi("c1"), gi("r2"), gi("c2"))
    val merge: MergeMode = g("merge") match {
      case "Opaque"      => MergeMode.Opaque
      case "Transparent" => MergeMode.Transparent
      case x             => sys.error(s"Unknown merge: $x")
    }
    val boundary: BoundaryMode = g("boundary") match {
      case "Clipping"  => BoundaryMode.Clipping
      case "Expanding" => BoundaryMode.Expanding
      case x           => sys.error(s"Unknown boundary: $x")
    }

    kind match {
      case "ROTATE90" =>
        val dir: RotationDir = g("dir") match {
          case "CW"  => RotationDir.CW
          case "CCW" => RotationDir.CCW
          case x     => sys.error(s"Unknown dir: $x")
        }
        val center = (gi("cr"), gi("cc"))
        Rotate90(sector, center, dir, merge, boundary)

      case "REFLECT_ROW" =>
        Reflect(sector, Axis.Row(gi("r0")), merge, boundary)

      case "REFLECT_COL" =>
        Reflect(sector, Axis.Col(gi("c0")), merge, boundary)

      case "REFLECT_MAIN" =>
        Reflect(sector, Axis.Diagonal(Axis.DiagonalKind.Main), merge, boundary)

      case "REFLECT_ANTI" =>
        Reflect(sector, Axis.Diagonal(Axis.DiagonalKind.Anti), merge, boundary)

      case "TRANSLATE" =>
        Translate(sector, dy = gi("dy"), dx = gi("dx"), merge, boundary)

      case "CENTRAL" =>
        val center = (gi("cr"), gi("cc"))
        CentralSymmetry(sector, center, merge, boundary)

      case other =>
        sys.error(s"Unknown isometry kind: $other")
    }
  }

  private def sanitize(name: String): String =
    name.replaceAll("[^a-zA-Z0-9_\\-\\.]", "_")
}
