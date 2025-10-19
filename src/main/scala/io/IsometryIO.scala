package io

import io.IsometryIO.{NamePrefix, ensureDir, pathFor}

import java.io.File
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.io.Source
import scala.util.Try
import logic.level.isometries.*
import logic.level.isometries.transformations.*
import model.Level

object IsometryIO {
  private val dir = Paths.get("isometries")
  private val NamePrefix = "# name:"


  /** Puna putanja za zadato "ime" bez ekstenzije. */
  def pathFor(nameNoExt: String): String = {
    ensureDir()
    dir.resolve(s"${sanitize(nameNoExt)}.txt").toString
  }

  /** Listaj sve .txt fajlove iz /isometries, vrati imena fajlova (sa ekstenzijom). */
  def listFiles(): Seq[String] = {
    if (!Files.exists(dir)) Files.createDirectories(dir)
    val f = dir.toFile
    Option(f.listFiles()).toSeq.flatten
      .filter(ff => ff.isFile && ff.getName.toLowerCase.endsWith(".txt"))
      .map(_.getName)
      .sorted
  }

  /** Učitaj kompozit izometrija iz fajla po imenu (npr. "SpiralCW.txt"). */
  def load(name: String): Either[String, Iso] = {
    val path = dir.resolve(name).toFile
    if (!path.exists()) return Left(s"File not found: $path")

    val src = Source.fromFile(path, "UTF-8")
    try {
      val lines = src.getLines().toVector
        .map(_.trim)
        .filter(l => l.nonEmpty && !l.startsWith("#"))

      val stepsEither: Either[String, List[Iso]] =
        lines.foldLeft(Right(Nil): Either[String, List[Iso]]) { (accE, line) =>
          for {
            acc <- accE
            iso <- parseLine(line)
          } yield acc :+ iso
        }

      stepsEither.map(LevelIsometries.composeAll)
    } finally src.close()
  }

  // --------------------------------------------------------------------------
  // Parsing
  // Format linije primer:
  // ROTATE90;r1=0;c1=0;r2=4;c2=4;cr=2;cc=2;dir=CW;merge=Opaque;boundary=Clipping
  //
  // REFLECT_ROW;r1=..;c1=..;r2=..;c2=..;r0=..
  // REFLECT_COL;...;c0=..
  // REFLECT_DIAG_MAIN;...
  // REFLECT_DIAG_ANTI;...
  // TRANSLATE;...;dy=..;dx=..;merge=...;boundary=...
  // CENTRAL;...;cr=..;cc=..
  // --------------------------------------------------------------------------
  private def parseLine(line: String): Either[String, Iso] = {
    val parts = line.split(";").map(_.trim).toList
    if (parts.isEmpty) return Left("Empty line")

    val op = parts.head.toUpperCase
    val kv = parts.tail.flatMap { p =>
      p.split("=", 2) match {
        case Array(k, v) => Some(k.trim.toLowerCase -> v.trim)
        case _           => None
      }
    }.toMap

    def int(name: String): Either[String, Int] =
      kv.get(name).toRight(s"Missing $name").flatMap(s => Try(s.toInt).toOption.toRight(s"Invalid int: $name=$s"))

    def str(name: String): Either[String, String] =
      kv.get(name).toRight(s"Missing $name")

    def sector(): Either[String, Sector] =
      for {
        r1 <- int("r1"); c1 <- int("c1"); r2 <- int("r2"); c2 <- int("c2")
      } yield Sector(r1, c1, r2, c2)

    def merge(): Either[String, MergeMode] =
      kv.get("merge").map(_.toLowerCase) match {
        case Some("opaque")      => Right(MergeMode.Opaque)
        case Some("transparent") => Right(MergeMode.Transparent)
        case None                => Right(MergeMode.Opaque) // default
        case Some(x)             => Left(s"Unknown merge=$x")
      }

    def boundary(): Either[String, BoundaryMode] =
      kv.get("boundary").map(_.toLowerCase) match {
        case Some("clipping")  => Right(BoundaryMode.Clipping)
        case Some("expanding") => Right(BoundaryMode.Expanding)
        case None              => Right(BoundaryMode.Clipping) // default
        case Some(x)           => Left(s"Unknown boundary=$x")
      }

    op match {
      case "ROTATE90" =>
        for {
          s   <- sector()
          cr  <- int("cr"); cc <- int("cc")
          d   <- str("dir").map(_.toUpperCase).flatMap {
            case "CW"  => Right(RotationDir.CW)
            case "CCW" => Right(RotationDir.CCW)
            case x     => Left(s"dir must be CW/CCW, got $x")
          }
          m   <- merge()
          b   <- boundary()
        } yield Rotate90(s, (cr, cc), d, m, b)

      case "REFLECT_ROW" =>
        for {
          s <- sector(); r0 <- int("r0"); m <- merge(); b <- boundary()
        } yield Reflect(s, Axis.Row(r0), m, b)

      case "REFLECT_COL" =>
        for {
          s <- sector(); c0 <- int("c0"); m <- merge(); b <- boundary()
        } yield Reflect(s, Axis.Col(c0), m, b)

      case "REFLECT_DIAG_MAIN" =>
        for {
          s <- sector(); m <- merge(); b <- boundary()
        } yield Reflect(s, Axis.Diagonal(Axis.DiagonalKind.Main), m, b)

      case "REFLECT_DIAG_ANTI" =>
        for {
          s <- sector(); m <- merge(); b <- boundary()
        } yield Reflect(s, Axis.Diagonal(Axis.DiagonalKind.Anti), m, b)

      case "TRANSLATE" =>
        for {
          s <- sector(); dy <- int("dy"); dx <- int("dx"); m <- merge(); b <- boundary()
        } yield Translate(s, dy, dx, m, b)

      case "CENTRAL" =>
        for {
          s <- sector(); cr <- int("cr"); cc <- int("cc"); m <- merge(); b <- boundary()
        } yield CentralSymmetry(s, (cr, cc), m, b)

      case other =>
        Left(s"Unknown op: $other")
    }
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

  private def ensureDir(): Unit =
    if (!Files.exists(dir)) Files.createDirectories(dir)

  private def sanitize(name: String): String =
    name.replaceAll("[^a-zA-Z0-9_\\-\\.]", "_")  
}
