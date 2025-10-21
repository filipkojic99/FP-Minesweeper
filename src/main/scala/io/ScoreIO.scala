// io/ScoreIO.scala
package io

import gui.services.ScoresFs
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters._

case class ScoreRow(
                     name: String,
                     score: Int,
                     timeSec: Long,
                     clicks: Int,
                     hints: Int,
                     levelFile: String
                   )

object ScoreIO {
  /** Appends in 6-field format: name;score;time;clicks;hints;levelFile */
  def append(diff: String, row: ScoreRow): Unit = {
    val p = ScoresFs.fileFor(diff)
    val line = s"${row.name};${row.score};${row.timeSec};${row.clicks};${row.hints};${row.levelFile}\n"
    Files.writeString(p, line, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  }

  /** Reads 6-field format. Lines starting with # or - are skipped (comments/separators). */
  def readAll(diff: String): Vector[ScoreRow] = {
    val p = ScoresFs.fileFor(diff)
    if (!Files.exists(p)) return Vector.empty
    Files.readAllLines(p).asScala.toVector.flatMap { ln =>
      val t = ln.trim
      if (t.isEmpty || t.startsWith("#") || t.startsWith("-")) None
      else {
        val parts = t.split(";", -1)
        if (parts.length != 6) None
        else {
          try Some(ScoreRow(
            name      = parts(0),
            score     = parts(1).toInt,
            timeSec   = parts(2).toLong,
            clicks    = parts(3).toInt,
            hints     = parts(4).toInt,
            levelFile = parts(5)
          )) catch { case _: Throwable => None }
        }
      }
    }
  }
}
