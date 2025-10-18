package gui.services

import java.nio.file.{Files, Path, Paths}

object ScoresFs {
  private val base: Path = Paths.get("scores")

  /** scores/beginner.txt, scores/intermediate.txt, scores/expert.txt */
  def fileFor(diff: String): Path = base.resolve(s"${diff.toLowerCase}.txt")
}
