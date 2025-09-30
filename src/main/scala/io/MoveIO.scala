package io

import scala.io.Source

object MoveIO {
  
  /** Parse one line L(r,c) or D(r,c). */
  def parseLine(line: String): Option[(Char, Int, Int)] = {
    val trimmed = line.trim
    if (trimmed.isEmpty || trimmed.startsWith("#")) return None

    // First char is the command (L or D)
    val kind = trimmed.charAt(0)

    // Extract inside of parentheses
    val inside = trimmed.drop(2).dropRight(1) // remove "L(" at start and ")" at end
    val parts = inside.split(",")
    if (parts.length != 2) {
      throw new IllegalArgumentException(s"Invalid move line: $line")
    }

    val r = parts(0).toInt - 1
    val c = parts(1).toInt - 1
    Some((kind, r, c))
  }

  /** Read moves from the file. */
  def readMoves(path: String): Vector[(Char, Int, Int)] = {
    val src = Source.fromFile(path)
    try {
      src.getLines().flatMap(parseLine).toVector
    } finally {
      src.close()
    }
  }

  /** Pretty-print a move for debugging or console output. */
  def pretty(move: (Char, Int, Int)): String = {
    val (k, r, c) = move
    s"$k(${r + 1},${c + 1})"
  }
}
