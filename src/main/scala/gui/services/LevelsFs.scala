package gui.services

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import logic.level.LevelDifficulty

object LevelsFs {

  /** Returns the names of .txt files from levels/<difficulty>/ (only names, without paths). */
  def listLevelFiles(d: LevelDifficulty): Seq[String] = {
    val dir = Paths.get("levels", folderName(d))
    if (!Files.exists(dir) || !Files.isDirectory(dir)) return Seq.empty

    Files.list(dir).iterator().asScala
      .filter(p => Files.isRegularFile(p))
      .map(_.getFileName.toString)
      .filter(_.toLowerCase.endsWith(".txt"))
      .toSeq
      .sorted
  }

  /** Full path to the level file. */
  def resolvePath(d: LevelDifficulty, fileName: String): String =
    Paths.get("levels", folderName(d), fileName).toString

  /** Returns the folder name for the given difficulty. */
  private def folderName(d: LevelDifficulty): String = d match {
    case LevelDifficulty.Beginner => "beginner"
    case LevelDifficulty.Intermediate => "intermediate"
    case LevelDifficulty.Expert => "expert"
  }
}
