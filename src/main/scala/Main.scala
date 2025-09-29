import io.LevelIO
import ops.BoardOps
import cli.Renderer

object Main extends App {
  val path = "levels/beginner/level1.txt"
  val raw = LevelIO.readLevel(path)
  val board = BoardOps.buildFromChars(raw)
  println(s"Board size: ${board.rows} x ${board.cols}")
  Renderer.printBoard(board)
}
