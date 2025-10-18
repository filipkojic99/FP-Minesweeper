package gui.screens

import java.awt.{BorderLayout, Color, FlowLayout}
import javax.swing.{JButton, JLabel, JOptionPane, JPanel, Timer}
import gui.widgets.BoardPanel
import model.{CellContent, GameState, GameStatus}
import logic.GameOps

final class GameScreen(
                        private var gs: GameState,
                        private val mkGame: () => GameState
                      ) extends JPanel(new BorderLayout()) {

  private val status = new JLabel("")
  private val btnReset = new JButton("Reset")

  private val top = new JPanel(new FlowLayout(FlowLayout.LEFT))
  top.add(btnReset)
  top.add(status)

  /** Board component that handles user input (left/right clicks). */
  private val board: BoardPanel = new BoardPanel(
    onLeftClick = (r, c) => {
      if (gs.status == GameStatus.InProgress) {
        board.setHintAt(None)
        val clickedIsMine = (gs.board.grid(r)(c).content == CellContent.Mine)

        gs = GameOps.reveal(gs, r, c)

        if (clickedIsMine && gs.status == GameStatus.Lost) {
          board.setExplodedAt(Some((r, c))) // save exploded mine for marking its background
        } else {
          board.setExplodedAt(None) // no explosion
        }

        refresh()
      }
    },
    onRightClick = (r, c) => {
      if (gs.status == GameStatus.InProgress) {
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

  /** Initializes the screen content. */
  refresh()
}
