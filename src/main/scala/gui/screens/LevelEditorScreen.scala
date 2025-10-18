package gui.screens

import java.awt.{BorderLayout, FlowLayout}
import javax.swing.*
import gui.widgets.BoardPanel
import logic.BoardOps
import logic.level.{ColEdge, LevelDifficulty, LevelOps, RowEdge}
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
    if (isIsometries) buildIsometriesPanel() // ostaje placeholder
    else buildBasicOpsPanel()

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
        val msg = s"OK - ${info.difficulty} | cells=${info.cells} | mines=${info.mines} | allowed=[${info.minMines}..${info.maxMines}]"
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
  private def renderFromLevel(showNumbers: Boolean = false): Unit = {
//    val chars: Vector[Vector[Char]] =
//      level.cells.map(_.map {
//        case CellContent.Mine  => '#'
//        case CellContent.Clear => '-'
//      })
//    val boardModel = BoardOps.buildFromChars(chars)
//    val allRevealed = Vector.fill(boardModel.rows)(Vector.fill(boardModel.cols)(CellState.Revealed))
//    val gs = GameState.newGame(boardModel).copy(state = allRevealed, clicks = 0, hintsUsed = 0)
//    board.render(gs)

    board.renderLevel(level, showNumbers) // ili toggle kasnije
  }

  private def wrapNorth(c: JComponent): JComponent = {
    val p = new JPanel(new BorderLayout())
    p.add(c, BorderLayout.NORTH)
    p
  }

  private def buildBasicOpsPanel(): JComponent = {
    val p = new JPanel(new java.awt.GridLayout(0, 1, 8, 8))
    p.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10))

    // --- Show numbers toggle ---
    val chkShowNums = new JCheckBox("Show numbers", false)
    chkShowNums.addActionListener(_ => renderFromLevel(showNumbers = chkShowNums.isSelected))
    p.add(chkShowNums)

    // --- Add row ---
    val addRowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnAddRowTop = new JButton("Add row (Top)")
    val btnAddRowBottom = new JButton("Add row (Bottom)")
    btnAddRowTop.addActionListener(_ => {
      level = LevelOps.addRow(level, RowEdge.Top); renderFromLevel(chkShowNums.isSelected)
    })
    btnAddRowBottom.addActionListener(_ => {
      level = LevelOps.addRow(level, RowEdge.Bottom); renderFromLevel(chkShowNums.isSelected)
    })
    addRowPanel.add(new JLabel("Rows:"));
    addRowPanel.add(btnAddRowTop);
    addRowPanel.add(btnAddRowBottom)
    p.add(addRowPanel)

    // --- Remove row ---
    val remRowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnRemRowTop = new JButton("Remove row (Top)")
    val btnRemRowBottom = new JButton("Remove row (Bottom)")
    btnRemRowTop.addActionListener(_ => {
      level = LevelOps.removeRow(level, RowEdge.Top); renderFromLevel(chkShowNums.isSelected)
    })
    btnRemRowBottom.addActionListener(_ => {
      level = LevelOps.removeRow(level, RowEdge.Bottom); renderFromLevel(chkShowNums.isSelected)
    })
    remRowPanel.add(new JLabel("Rows:"));
    remRowPanel.add(btnRemRowTop);
    remRowPanel.add(btnRemRowBottom)
    p.add(remRowPanel)

    // --- Add col ---
    val addColPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnAddColLeft = new JButton("Add col (Left)")
    val btnAddColRight = new JButton("Add col (Right)")
    btnAddColLeft.addActionListener(_ => {
      level = LevelOps.addCol(level, ColEdge.Left); renderFromLevel(chkShowNums.isSelected)
    })
    btnAddColRight.addActionListener(_ => {
      level = LevelOps.addCol(level, ColEdge.Right); renderFromLevel(chkShowNums.isSelected)
    })
    addColPanel.add(new JLabel("Cols:"));
    addColPanel.add(btnAddColLeft);
    addColPanel.add(btnAddColRight)
    p.add(addColPanel)

    // --- Remove col ---
    val remColPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnRemColLeft = new JButton("Remove col (Left)")
    val btnRemColRight = new JButton("Remove col (Right)")
    btnRemColLeft.addActionListener(_ => {
      level = LevelOps.removeCol(level, ColEdge.Left); renderFromLevel(chkShowNums.isSelected)
    })
    btnRemColRight.addActionListener(_ => {
      level = LevelOps.removeCol(level, ColEdge.Right); renderFromLevel(chkShowNums.isSelected)
    })
    remColPanel.add(new JLabel("Cols:"));
    remColPanel.add(btnRemColLeft);
    remColPanel.add(btnRemColRight)
    p.add(remColPanel)

    // --- Toggle by coords (r,c) ---
    val togglePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfR = new JTextField(3)
    val tfC = new JTextField(3)
    val btnToggle = new JButton("Toggle (r,c)")
    btnToggle.addActionListener(_ => {
      parse2(tfR.getText, tfC.getText) match {
        case Some((r, c)) =>
          LevelOps.toggleAt(level, r, c) match {
            case Some(lv) => level = lv; renderFromLevel(chkShowNums.isSelected)
            case None => JOptionPane.showMessageDialog(this, "Out of bounds.", "Toggle", JOptionPane.WARNING_MESSAGE)
          }
        case None => JOptionPane.showMessageDialog(this, "Enter integers for r and c.", "Toggle", JOptionPane.WARNING_MESSAGE)
      }
    })
    togglePanel.add(new JLabel("r:"));
    togglePanel.add(tfR)
    togglePanel.add(new JLabel("c:"));
    togglePanel.add(tfC)
    togglePanel.add(btnToggle)
    p.add(togglePanel)

    // --- Clear sector (r1,c1,r2,c2) ---
    val clearPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfR1 = new JTextField(3);
    val tfC1 = new JTextField(3)
    val tfR2 = new JTextField(3);
    val tfC2 = new JTextField(3)
    val btnClear = new JButton("Clear sector")
    btnClear.addActionListener(_ => {
      parse4(tfR1.getText, tfC1.getText, tfR2.getText, tfC2.getText) match {
        case Some((r1, c1, r2, c2)) =>
          LevelOps.clearRect(level, r1, c1, r2, c2) match {
            case Some(lv) => level = lv; renderFromLevel(chkShowNums.isSelected)
            case None => JOptionPane.showMessageDialog(this, "Invalid rectangle / out of bounds.", "Clear sector", JOptionPane.WARNING_MESSAGE)
          }
        case None =>
          JOptionPane.showMessageDialog(this, "Enter integers for r1,c1,r2,c2.", "Clear sector", JOptionPane.WARNING_MESSAGE)
      }
    })
    clearPanel.add(new JLabel("r1:"));
    clearPanel.add(tfR1)
    clearPanel.add(new JLabel("c1:"));
    clearPanel.add(tfC1)
    clearPanel.add(new JLabel("r2:"));
    clearPanel.add(tfR2)
    clearPanel.add(new JLabel("c2:"));
    clearPanel.add(tfC2)
    clearPanel.add(btnClear)
    p.add(clearPanel)

    p
  }

  private def buildIsometriesPanel(): JComponent = {
    val p = new JPanel(new java.awt.GridLayout(0, 1, 8, 8))
    p.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10))
    p.add(new JLabel("Isometries panel (soon)"))
    p
  }

  private def parseInt(s: String): Option[Int] =
    try Some(s.trim.toInt) catch {
      case _: Throwable => None
    }

  private def parse2(a: String, b: String): Option[(Int, Int)] =
    for {x <- parseInt(a); y <- parseInt(b)} yield (x, y)

  private def parse4(a: String, b: String, c: String, d: String): Option[(Int, Int, Int, Int)] =
    for {
      x1 <- parseInt(a); y1 <- parseInt(b)
      x2 <- parseInt(c); y2 <- parseInt(d)
    } yield (x1, y1, x2, y2)


}
