package gui.services

import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters._

object MovesFs {
  private val base: Path = Paths.get("moves")

  /** Returns the names of .txt files from /moves (only names, without paths). */
  def listMoveFiles(): Vector[String] = {
    if (!Files.exists(base)) return Vector.empty
    Files.list(base)
      .iterator()
      .asScala
      .toVector
      .filter(p => Files.isRegularFile(p) && p.getFileName.toString.toLowerCase.endsWith(".txt"))
      .map(_.getFileName.toString)
      .sorted
  }

  /** Full path to the moves file. */
  def resolvePath(fileName: String): Path = base.resolve(fileName)
}
