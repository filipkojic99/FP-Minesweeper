package gui

import javax.swing.SwingUtilities

object App extends App {
  SwingUtilities.invokeLater(() => {
    val frame = new MainFrame
    frame.showBlank()
  })
}