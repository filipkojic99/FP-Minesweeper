package gui.screens

import java.awt.FlowLayout
import javax.swing.{JButton, JOptionPane, JPanel}
import gui.MainFrame
import gui.services.{Dialogs, LevelsFs, MovesFs, SavesFs}
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
                  val levelPath = LevelsFs.resolvePath(diff, fileName).toString
                  val gs0 = mkGame(diff, fileName)
                  frame.showGame(gs0, () => mkGame(diff, fileName), levelPath)

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

              val savePath = SavesFs.resolvePath(fileName).toString
              val (gsLoaded, levelPath) = io.GameIO.load(savePath)

              val mkFromLevelPath = () => {
                val chars = io.LevelIO.readLevel(levelPath)
                val board = logic.BoardOps.buildFromChars(chars)
                model.GameState.newGame(board)
              }

              frame.showGame(gsLoaded, mkFromLevelPath, levelPath)

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

  /** Handle moves insertion. */
  btnMoves.addActionListener { _ =>
    val files = MovesFs.listMoveFiles()
    if (files.isEmpty) {
      javax.swing.JOptionPane.showMessageDialog(
        this, "No .txt files found in /moves.", "Insert moves",
        javax.swing.JOptionPane.WARNING_MESSAGE
      )
    } else {
      Dialogs.chooseLevel(this, files) match {
        case Some(fileName) => frame.requestInsertMoves(fileName)
        case None           => println("[GUI] Insert moves canceled")
      }
    }
  }

  /** Handle progress saving. */
  btnSave.addActionListener { _ =>
    val name = JOptionPane.showInputDialog(
      this,
      "Enter save file name (without extension):",
      "Save game",
      JOptionPane.PLAIN_MESSAGE
    )

    if (name != null) {
      val trimmed = name.trim
      if (trimmed.nonEmpty) {
        frame.requestSaveGameWithName(s"$trimmed.txt")
      }
    }
  }

  btnScores.addActionListener { _ =>
    Dialogs.chooseDifficulty(this) match {
      case Some(diff) =>
        val diffStr = diff.toString.toLowerCase
        val all = io.ScoreIO.readAll(diffStr)

        val grouped = all.groupBy(_.levelFile).toSeq.sortBy(_._1) // po imenu levela
        val text =
          if (all.isEmpty) "No scores yet."
          else grouped.map { case (level, items) =>
            val top = items.sortBy(r => (r.score, r.timeSec, r.clicks, r.hints)).take(10) // manji bolji
            val header = s"----- $level -----"
            val lines = top.zipWithIndex.map { case (r, i) =>
              f"${i + 1}%2d. ${r.name} — score:${r.score}%d  time:${r.timeSec}%d s  clicks:${r.clicks}%d  hints:${r.hints}%d"
            }.mkString("\n")
            s"$header\n$lines"
          }.mkString("\n\n")

        javax.swing.JOptionPane.showMessageDialog(
          this, text, s"Best scores – ${diff.toString}", javax.swing.JOptionPane.INFORMATION_MESSAGE
        )
      case None => ()
    }
  }


  btnCreate.addActionListener(_ => println("Create level clicked"))

  /* Create new game. */
  private def mkGame(diff: LevelDifficulty, fileName: String): GameState = {
    val path = LevelsFs.resolvePath(diff, fileName)
    val chars = LevelIO.readLevel(path)
    val board: Board = BoardOps.buildFromChars(chars)
    GameState.newGame(board)
  }
}
