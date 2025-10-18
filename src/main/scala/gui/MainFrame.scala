package gui

import java.awt.BorderLayout
import javax.swing.{JComponent, JFrame, JPanel}
import javax.swing.WindowConstants
import gui.screens.{MainMenuScreen, GameScreen}
import model.GameState

class MainFrame extends JFrame("Minesweeper") {

  private val centerHolder = new JPanel(new BorderLayout())

  private var currentGame: Option[GameScreen] = None

  setLayout(new BorderLayout())
  add(new MainMenuScreen(this), BorderLayout.NORTH) // menu location
  add(centerHolder, BorderLayout.CENTER)

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setSize(900, 650)
  setLocationRelativeTo(null)
  setVisible(true)

  /** Displays the GameScreen for the current game state. */
  def showGame(gs: GameState, mkGame: () => GameState): Unit =
    val g = new GameScreen(gs, mkGame)
    currentGame = Some(g)
    setCenter(g)

  /** Clears the center panel (e.g., when returning to menu). */
  def showBlank(): Unit =
    setCenter(new JPanel())

  def requestHint(): Unit =
    currentGame match {
      case Some(g) => g.showHint()
      case None => javax.swing.JOptionPane.showMessageDialog(
        this, "Please start the game first.", "Hint", javax.swing.JOptionPane.WARNING_MESSAGE
      )
    }

  /** Replaces the center component with the given panel. */
  private def setCenter(c: JComponent): Unit = {
    centerHolder.removeAll()
    centerHolder.add(c, BorderLayout.CENTER)
    centerHolder.revalidate()
    centerHolder.repaint()
  }
}
