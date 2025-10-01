package io

import logic.BoardOps
import model.*

import java.io.PrintWriter
import scala.io.Source

object GameIO {
  /** Save current game state into a file. */
  def save(path: String, gs: GameState, levelPath: String): Unit = {
    if (gs.status != GameStatus.InProgress) {
      throw new IllegalStateException("Cannot save a finished game.")
    }

    val pw = new PrintWriter(path)
    try {
      pw.println(s"LEVEL=$levelPath")
      pw.println(s"STATUS=${gs.status}")
      pw.println(s"CLICKS=${gs.clicks}")
      pw.println(s"STARTED=${gs.startedAtMs}")
      pw.println(s"ELAPSED=${gs.elapsedSeconds()}")
      pw.println(s"HINTS=${gs.hintsUsed}")

      for (row <- gs.state) {
        val line = row.map {
          case CellState.Hidden => 'H'
          case CellState.Revealed => 'R'
          case CellState.Flagged => 'F'
        }.mkString
        pw.println(line)
      }
    } finally pw.close()
  }

  /** Load a saved game. Returns (GameState, levelPath). */
  def load(path: String): (GameState, String) = {
    val src = Source.fromFile(path)
    try {
      val lines = src.getLines().toVector
      val header = lines.takeWhile(_.contains("="))
      val grid = lines.drop(header.length)

      val kv = header.map { line =>
        val Array(k, v) = line.split("=", 2)
        k -> v
      }.toMap

      val levelPath = kv("LEVEL")
      val status = GameStatus.valueOf(kv("STATUS"))
      val clicks = kv("CLICKS").toInt
      val hints = kv("HINTS").toInt

      val now = System.currentTimeMillis()
      
      val elapsedSavedSec: Long = kv.get("ELAPSED")
        .map(_.toLong)
        .orElse {
          kv.get("STARTED").map { startedStr =>
            val started = startedStr.toLong
            val ms = (now - started).max(0L)
            ms / 1000L
          }
        }
        .getOrElse(0L)

      val raw = LevelIO.readLevel(levelPath)
      val board = BoardOps.buildFromChars(raw)

      val state: Vector[Vector[CellState]] = grid.map { line =>
        line.toVector.map {
          case 'H' => CellState.Hidden
          case 'R' => CellState.Revealed
          case 'F' => CellState.Flagged
          case ch => throw new IllegalArgumentException(s"Invalid cell state: $ch")
        }
      }

      val gs = GameState(
        board = board,
        state = state,
        status = status,
        clicks = clicks,
        startedAtMs = now,
        endedAtMs = None,
        hintsUsed = hints,
        score = None,
        elapsedSavedSec = elapsedSavedSec
      )

      (gs, levelPath)
    } finally src.close()
  }
}
