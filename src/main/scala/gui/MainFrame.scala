package gui

import java.awt.CardLayout
import javax.swing.{JFrame, JPanel}
import gui.screens.MainMenuScreen
import javax.swing.WindowConstants

class MainFrame extends JFrame("Minesweeper") {
  private val cards = new CardLayout()
  private val root = new JPanel(cards)

  setContentPane(root)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setSize(900, 650)
  setLocationRelativeTo(null)
  setVisible(true)

  def showMenu(): Unit = {
    val menu = new MainMenuScreen(this)
    root.add(menu, "menu")
    cards.show(root, "menu")
  }
}
