package gui.screens

import java.awt.FlowLayout
import javax.swing.{JButton, JOptionPane, JPanel}
import gui.MainFrame
import gui.services.{Dialogs, LevelsFs, SavesFs}
import io.LevelIO
import logic.BoardOps
import logic.level.LevelDifficulty
import model.{Board, GameState}

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

  /** Handles the Start Game button click - shows dialogs to start or resume. */
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
                  val gs0 = mkGame(diff, fileName)
                  frame.showGame(gs0, () => mkGame(diff, fileName))

                case None => println("[GUI] Canceled at chooseLevel")
              }
            }
          case None => println("[GUI] Canceled at chooseDifficulty")
        }

      case Some(Dialogs.ResumeOld) =>
        val saves = SavesFs.listSavedGames()
        if (saves.isEmpty) {
          javax.swing.JOptionPane.showMessageDialog(
            this,
            "No saved games found in /saves folder.",
            "Resume game",
            javax.swing.JOptionPane.WARNING_MESSAGE
          )
        } else {
          Dialogs.chooseLevel(this, saves) match {
            case Some(fileName) =>
              println(s"[GUI] Resume old game → file=$fileName")

            // later:
            // val (gs, levelPath) = io.GameIO.load(SavesFs.resolvePath(fileName))
            // val adapter = new CoreGameAdapter(gs, () => gs)
            // frame.showGame(adapter)

            case None =>
              println("[GUI] Resume old canceled")
          }
        }


      case None =>
        println("[GUI] Start canceled")
    }
  }

  /** Handles the Hint button click. */
  btnHint.addActionListener(_ => frame.requestHint())

  btnMoves.addActionListener(_ => println("Insert moves clicked"))
  btnSave.addActionListener(_ => println("Save clicked"))
  btnCreate.addActionListener(_ => println("Create level clicked"))
  btnScores.addActionListener(_ => println("Best scores clicked"))

  /* Create new game. */
  private def mkGame(diff: LevelDifficulty, fileName: String): GameState = {
    val path = LevelsFs.resolvePath(diff, fileName)
    val chars = LevelIO.readLevel(path)
    val board: Board = BoardOps.buildFromChars(chars)
    GameState.newGame(board)
  }
}
