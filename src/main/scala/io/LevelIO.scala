package io

import model.{CellContent, Level}
import logic.level.{LevelDifficulty, LevelValidate}

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.Source

object LevelIO {

  /** Read level from the file. */
  def readLevel(path: String): Vector[Vector[Char]] = {
    val src = Source.fromFile(path)
    try {
      src.getLines().toVector.map(_.toVector)
    } finally {
      src.close()
    }
  }

  /** Attempts to detect the difficulty from the level content and save it to the corresponding folder.
      Returns Right(path) if successful; Left(errorMsg) if the level is invalid (e.g., out of range).
   */
  def saveLevelAuto(level: Level): Either[String, String] = {
    LevelValidate.validate(level) match {
      case Right(info) =>
        Right(saveLevel(level, info.difficulty))
      case Left(errs) =>
        val msg = "Level is not valid:\n" + errs.map(_.toString).mkString("• ", "\n• ", "")
        Left(msg)
    }
  }

  /** Save level to a text file in folder based on difficulty. */
  def saveLevel(level: Level, difficulty: LevelDifficulty): String = {
    val folderPath = difficulty match {
      case LevelDifficulty.Beginner => "levels/beginner"
      case LevelDifficulty.Intermediate => "levels/intermediate"
      case LevelDifficulty.Expert => "levels/expert"
    }

    val folder = new File(folderPath)
    if (!folder.exists()) folder.mkdirs()

    // Generate file name with timestamp: level_2025-10-07_20-43.txt
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm"))
    val fileName = s"level_$timestamp.txt"
    val filePath = s"$folderPath/$fileName"

    val pw = new PrintWriter(new File(filePath))
    try {
      level.cells.foreach { row =>
        val line = row.map {
          case CellContent.Mine => '#'
          case CellContent.Clear => '-'
        }.mkString
        pw.println(line)
      }
    } finally {
      pw.close()
    }

    filePath // for display
  }
}
