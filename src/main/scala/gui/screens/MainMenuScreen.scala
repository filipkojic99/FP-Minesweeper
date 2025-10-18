package gui.screens

import java.awt.FlowLayout
import javax.swing.{JButton, JOptionPane, JPanel}
import gui.MainFrame
import gui.services.{Dialogs, LevelsFs}
import logic.level.LevelDifficulty

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

  btnStart.addActionListener { _ =>
    Dialogs.chooseStartOrResume(this) match {
      case Some(Dialogs.StartNew) =>
        Dialogs.chooseDifficulty(this) match {
          case Some(diff) =>
            val levels = LevelsFs.listLevelFiles(diff)
            if (levels.isEmpty) {
              JOptionPane.showMessageDialog(
                this,
                s"No .txt levels found in levels/${diff.toString.toLowerCase}",
                "No levels",
                JOptionPane.WARNING_MESSAGE
              )
            } else {
              Dialogs.chooseLevel(this, levels) match {
                case Some(fileName) =>
                  println(s"[GUI] New game → diff=$diff, level=$fileName")

                // (opciono) odmah pokreni igru:
                // val gs = mkGame(diff, fileName)
                // val adapter = new CoreGameAdapter(gs, () => mkGame(diff, fileName))
                // frame.showGame(adapter)

                case None => println("[GUI] Canceled at chooseLevel")
              }
            }
          case None => println("[GUI] Canceled at chooseDifficulty")
        }

      case Some(Dialogs.ResumeOld) =>
        println("[GUI] Resume old game chosen")
      // ovde ćemo kasnije povezati GameIO.load(...)

      case None =>
        println("[GUI] Start canceled")
    }
  }


  btnMoves.addActionListener(_ => println("Insert moves clicked"))
  btnHint.addActionListener(_ => println("Hint clicked"))
  btnSave.addActionListener(_ => println("Save clicked"))
  btnCreate.addActionListener(_ => println("Create level clicked"))
  btnScores.addActionListener(_ => println("Best scores clicked"))
}
