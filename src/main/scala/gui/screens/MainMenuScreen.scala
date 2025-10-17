package gui.screens

import java.awt.FlowLayout
import javax.swing.{JButton, JPanel}
import gui.MainFrame

class MainMenuScreen(frame: MainFrame) extends JPanel(new FlowLayout(FlowLayout.LEFT)) {

  setLayout(new FlowLayout(FlowLayout.CENTER, 10, 20))

  private val btnStart  = new JButton("Start game")
  private val btnMoves  = new JButton("Insert moves")
  private val btnHint   = new JButton("Hint")
  private val btnSave   = new JButton("Save game")
  private val btnCreate = new JButton("Create level")
  private val btnScores = new JButton("Best scores")

  add(btnStart)
  add(btnMoves)
  add(btnHint)
  add(btnSave)
  add(btnCreate)
  add(btnScores)

  btnStart.addActionListener(_ => println("Start game clicked"))
  btnMoves.addActionListener(_ => println("Insert moves clicked"))
  btnHint.addActionListener(_ => println("Hint clicked"))
  btnSave.addActionListener(_ => println("Save clicked"))
  btnCreate.addActionListener(_ => println("Create level clicked"))
  btnScores.addActionListener(_ => println("Best scores clicked"))
}
