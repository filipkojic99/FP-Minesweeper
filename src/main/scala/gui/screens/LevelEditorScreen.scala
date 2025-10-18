package gui.screens

import java.awt.{BorderLayout, FlowLayout}
import javax.swing._
import gui.widgets.BoardPanel
import logic.BoardOps
import logic.level.{LevelDifficulty, LevelOps}
import model.{CellContent, CellState, GameState, Level}

final class LevelEditorScreen(
                               isIsometries: Boolean,
                               levelPath: String,
                               difficulty: LevelDifficulty
                             ) extends JPanel(new BorderLayout()) {

  private var level: Level = LevelOps.buildLevel(levelPath)

  private val board = new BoardPanel(
    onLeftClick = (r, c) => {
      LevelOps.toggleAt(level, r, c).foreach { lv =>
        level = lv
        renderFromLevel()
      }
    },
    onRightClick = (_, _) => {
      // for isometries later
    }
  )

  private val btnValidate = new JButton("Validate")
  private val btnSave     = new JButton("Save level")
  private val btnClose    = new JButton("Close")

  private val top = new JPanel(new FlowLayout(FlowLayout.LEFT))
  top.add(new JLabel(s"Editing: $levelPath  |  Mode: " + (if (isIsometries) "Isometries" else "Basic")))
  top.add(btnValidate); top.add(btnSave); top.add(btnClose)

  // Basic/Isometries UI
  private val rightPanel: JComponent =
    if (isIsometries) new JLabel("Isometries panel (soon)")
    else new JLabel("Basic operations panel (soon)")

  setLayout(new BorderLayout())
  add(top, BorderLayout.NORTH)
  add(board, BorderLayout.CENTER)
  add(wrapNorth(rightPanel), BorderLayout.EAST)

  // actions
  btnValidate.addActionListener(_ => {
    logic.level.LevelValidate.validate(level) match {
      case Left(errs) =>
        val msg = errs.map(_.toString).mkString("• ", "\n• ", "")
        JOptionPane.showMessageDialog(this, msg, "Validation", JOptionPane.WARNING_MESSAGE)
      case Right(info) =>
        val msg = s"OK — ${info.difficulty} | cells=${info.cells} | mines=${info.mines} | allowed=[${info.minMines}..${info.maxMines}]"
        JOptionPane.showMessageDialog(this, msg, "Validation", JOptionPane.INFORMATION_MESSAGE)
    }
  })

  btnSave.addActionListener(_ => {
    io.LevelIO.saveLevelAuto(level) match {
      case Right(path) =>
        JOptionPane.showMessageDialog(this, s"Saved to: $path", "Save level", JOptionPane.INFORMATION_MESSAGE)
      case Left(err) =>
        JOptionPane.showMessageDialog(this, err, "Save level - invalid level", JOptionPane.WARNING_MESSAGE)
    }
  })

  btnClose.addActionListener(_ => {
    val win = SwingUtilities.getWindowAncestor(this)
    win match {
      case f: gui.MainFrame => f.showBlank()
      case _ => this.getParent.remove(this); this.revalidate(); this.repaint()
    }
  })

  // inicijalni prikaz
  renderFromLevel()

  /** Adapter: Level -> GameState (sve Revealed) za BoardPanel.render */
  private def renderFromLevel(): Unit = {
//    val chars: Vector[Vector[Char]] =
//      level.cells.map(_.map {
//        case CellContent.Mine  => '#'
//        case CellContent.Clear => '-'
//      })
//    val boardModel = BoardOps.buildFromChars(chars)
//    val allRevealed = Vector.fill(boardModel.rows)(Vector.fill(boardModel.cols)(CellState.Revealed))
//    val gs = GameState.newGame(boardModel).copy(state = allRevealed, clicks = 0, hintsUsed = 0)
//    board.render(gs)

    board.renderLevel(level, showNumbers = false) // ili toggle kasnije
  }

  private def wrapNorth(c: JComponent): JComponent = {
    val p = new JPanel(new BorderLayout())
    p.add(c, BorderLayout.NORTH)
    p
  }
}
