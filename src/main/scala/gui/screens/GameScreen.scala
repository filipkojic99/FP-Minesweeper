package gui.screens

import java.awt.{BorderLayout, Color, FlowLayout}
import javax.swing.{JButton, JLabel, JPanel, Timer}
import gui.widgets.BoardPanel
import model.{CellContent, GameState, GameStatus}
import logic.GameOps

final class GameScreen(
                        private var gs: GameState,
                        private val mkGame: () => GameState
                      ) extends JPanel(new BorderLayout()) {

  private val status = new JLabel("")
  private val btnReset = new JButton("Reset")
  // later: val btnHint  = new JButton("Hint")

  private val top = new JPanel(new FlowLayout(FlowLayout.LEFT))
  top.add(btnReset)
  // later: top.add(btnHint)
  top.add(status)

  /** Board component that handles user input (left/right clicks). */
  private val board: BoardPanel = new BoardPanel(
    onLeftClick = (r, c) => {
      if (gs.status == GameStatus.InProgress) {
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
        gs = GameOps.toggleFlag(gs, r, c)
        refresh()
      }
    }
  )

  /** Resets the current game to its initial state. */
  btnReset.addActionListener(_ => {
    gs = mkGame()
    board.setExplodedAt(None)
    refresh()
  })

  // later:
  // btnHint.addActionListener(_ => {
  //   val moveOpt = logic.GameOps.computeHint(gs)
  //   gs = logic.GameOps.applyHint(gs, moveOpt)
  //   refresh()
  // })

  add(top, BorderLayout.NORTH)
  add(board, BorderLayout.CENTER)

  /** Periodically updates the time and status label. */
  private val timer = new Timer(1000, _ => updateStatusLabel())
  timer.setRepeats(true)
  timer.start()

  /** Updates the text in the status bar (status, clicks, time). */
  private def updateStatusLabel(): Unit = {
    val t = gs.elapsedSeconds()
    status.setText(s"Status: ${gs.status} | Clicks: ${gs.clicks} | Time: ${t}s")
  }

  /** Redraws the board and refreshes all visual data. */
  def refresh(): Unit = {
    updateStatusLabel()
    board.render(gs)
  }

  /** Initializes the screen content. */
  refresh()
}
