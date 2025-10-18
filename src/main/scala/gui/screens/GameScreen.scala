package gui.screens

import gui.services.{MovesFs, SavesFs}

import java.awt.{BorderLayout, Color, FlowLayout}
import javax.swing.{JButton, JLabel, JOptionPane, JPanel, Timer}
import gui.widgets.BoardPanel
import io.{GameIO, MoveIO, ScoreIO, ScoreRow}
import model.{CellContent, CellState, GameState, GameStatus}
import logic.GameOps

final class GameScreen(
                        private var gs: GameState,
                        private val mkGame: () => GameState,
                        private val levelPath: String
                      ) extends JPanel(new BorderLayout()) {

  private val status = new JLabel("")
  private val btnReset = new JButton("Reset")

  private val top = new JPanel(new FlowLayout(FlowLayout.LEFT))
  top.add(btnReset)
  top.add(status)

  /** Board component that handles user input (left/right clicks). */
  private val board: BoardPanel = new BoardPanel(
    onLeftClick = (r, c) => {
      if (gs.status == GameStatus.InProgress && gs.state(r)(c) == CellState.Hidden) {
        board.setHintAt(None)
        val clickedIsMine = (gs.board.grid(r)(c).content == CellContent.Mine)

        val prev = gs.status
        gs = GameOps.reveal(gs, r, c)

        if (clickedIsMine && gs.status == GameStatus.Lost) {
          board.setExplodedAt(Some((r, c)))
        } else {
          board.setExplodedAt(None)
        }

        refresh()
        if (prev != GameStatus.Won && gs.status == GameStatus.Won) onWinSaveScore()
      }
    },
    onRightClick = (r, c) => {
      if (
        gs.status == GameStatus.InProgress &&
          (gs.state(r)(c) == CellState.Hidden || gs.state(r)(c) == CellState.Flagged)
      ) {
        board.setHintAt(None)
        gs = GameOps.toggleFlag(gs, r, c)
        refresh()
      }
    }
  )

  /** Resets the current game to its initial state. */
  btnReset.addActionListener(_ => {
    gs = mkGame()
    board.setExplodedAt(None)
    board.setHintAt(None)
    refresh()
  })

  add(top, BorderLayout.NORTH)
  add(board, BorderLayout.CENTER)

  /** Periodically updates the time and status label. */
  private val timer = new Timer(1000, _ => updateStatusLabel())
  timer.setRepeats(true)
  timer.start()

  def shutdown(): Unit = timer.stop()

  override def removeNotify(): Unit = {
    try timer.stop() finally super.removeNotify()
  }

  override def addNotify(): Unit = {
    super.addNotify()
    if (!timer.isRunning) timer.start()
  }

  /** Updates the text in the status bar (status, clicks, time). */
  private def updateStatusLabel(): Unit = {
    val t = gs.elapsedSeconds()
    status.setText(s"Status: ${gs.status} | Clicks: ${gs.clicks} | Hints used: ${gs.hintsUsed} | Time: ${t}s")
  }

  /** Redraws the board and refreshes all visual data. */
  def refresh(): Unit = {
    updateStatusLabel()
    board.render(gs)
  }

  /** Compute and display hint . */
  def showHint(): Unit = {
    if (gs.status != GameStatus.InProgress) {
      JOptionPane.showMessageDialog(this, "Game is not in progress.", "Hint", JOptionPane.INFORMATION_MESSAGE)
      return
    }
    val moveOpt = GameOps.computeHint(gs)
    moveOpt match {
      case Some((kind, x, y)) =>
        // increment hint counter
        gs = gs.copy(hintsUsed = gs.hintsUsed + 1)
        // Message for the player
        val moveStr = s"$kind(${x + 1},${y + 1})"
        JOptionPane.showMessageDialog(this, s"Move suggestion: $moveStr", "Hint", JOptionPane.INFORMATION_MESSAGE)

        // save hinted field
        board.setHintAt(Some((x, y)))
        refresh()

      case None =>
        JOptionPane.showMessageDialog(this, "No clear hint.", "Hint", JOptionPane.INFORMATION_MESSAGE)
    }
  }

  /** Read and apply moves from .txt file . */
  def onMovesFileChosen(fileName: String): Unit = {
    try {
      if (gs.status != GameStatus.InProgress) {
        JOptionPane.showMessageDialog(this, "Game is not in progress.", "Insert moves",
          JOptionPane.WARNING_MESSAGE)
        return
      }

      val path = MovesFs.resolvePath(fileName).toString
      val moves = MoveIO.readMoves(path) // Vector[(Char, Int, Int)]

      if (moves.isEmpty) {
        JOptionPane.showMessageDialog(this, s"No moves in $fileName.", "Insert moves", JOptionPane.INFORMATION_MESSAGE)
        return
      }

      board.setHintAt(None)
      board.setExplodedAt(None)

      // apply read moves
      gs = moves.foldLeft(gs) { (acc, m) => GameOps.applyMove(acc, m) }

      JOptionPane.showMessageDialog(this, s"Applied ${moves.size} moves from $fileName.", "Insert moves",
        JOptionPane.INFORMATION_MESSAGE)

      refresh()
    } catch {
      case ex: Throwable =>
        JOptionPane.showMessageDialog(this, s"Failed to load moves: ${ex.getMessage}",
          "Insert moves", JOptionPane.ERROR_MESSAGE)
    }
  }

  /** Save game in /saves directory . */
  def onSaveGame(fileName: String): Unit = {
    try {
      val path = SavesFs.resolvePath(fileName).toString

      if (gs.status != GameStatus.InProgress) {
        JOptionPane.showMessageDialog(this, "Cannot save a finished game.", "Save game",
          JOptionPane.WARNING_MESSAGE)
        return
      }

      GameIO.save(path, gs, levelPath)
      JOptionPane.showMessageDialog(this, s"Game saved to $path.", "Save game",
        JOptionPane.INFORMATION_MESSAGE)
    } catch {
      case ex: Throwable =>
        JOptionPane.showMessageDialog(this, s"Failed to save game: ${ex.getMessage}",
          "Save game", JOptionPane.ERROR_MESSAGE)
    }
  }

  private def difficultyFromLevelPath(path: String): String = {
    val s = path.toLowerCase
    if (s.contains("beginner")) "beginner"
    else if (s.contains("intermediate")) "intermediate"
    else if (s.contains("expert")) "expert"
    else "unknown"
  }

  private def baseName(path: String): String = new java.io.File(path).getName

  private def onWinSaveScore(): Unit = {
    val name = javax.swing.JOptionPane.showInputDialog(
      this, "You won! Enter your name for the leaderboard:", "Victory",
      javax.swing.JOptionPane.PLAIN_MESSAGE
    )
    if (name == null || name.trim.isEmpty) return

    val diff = difficultyFromLevelPath(levelPath)
    val row = ScoreRow(
      name = name.trim,
      score = gs.score.getOrElse(0), // manji = bolji
      timeSec = gs.elapsedSeconds(),
      clicks = gs.clicks,
      hints = gs.hintsUsed,
      levelFile = baseName(levelPath)
    )
    io.ScoreIO.append(diff, row)

    javax.swing.JOptionPane.showMessageDialog(
      this,
      s"Saved: ${row.name} — score:${row.score}  time:${row.timeSec}s  clicks:${row.clicks}  hints:${row.hints}  [${row.levelFile}]",
      "Victory",
      javax.swing.JOptionPane.INFORMATION_MESSAGE
    )
  }

  /** Initializes the screen content. */
  refresh()
}
