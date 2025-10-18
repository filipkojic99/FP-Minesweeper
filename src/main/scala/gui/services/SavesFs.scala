package gui.services

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object SavesFs {

  /** Returns the names of .txt files from /saves (only names, without paths). */
  def listSavedGames(): Seq[String] = {
    val dir = Paths.get("saves")
    if (!Files.exists(dir) || !Files.isDirectory(dir)) return Seq.empty

    Files.list(dir).iterator().asScala
      .filter(p => Files.isRegularFile(p))
      .map(_.getFileName.toString)
      .filter(_.toLowerCase.endsWith(".txt"))
      .toSeq
      .sorted
  }

  /** Full path to the saved game file. */
  def resolvePath(fileName: String): String =
    Paths.get("saves", fileName).toString
}
