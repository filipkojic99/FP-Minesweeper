package io

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
}
