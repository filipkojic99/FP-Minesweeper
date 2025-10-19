package gui.screens

import java.awt.{BorderLayout, FlowLayout}
import javax.swing.*
import gui.widgets.BoardPanel
import logic.BoardOps
import logic.level.isometries.{BoundaryMode, Iso, IsoOps, LevelIsometries, MergeMode, Sector}
import logic.level.{ColEdge, LevelDifficulty, LevelOps, RowEdge}
import model.{CellContent, CellState, GameState, Level}

final class LevelEditorScreen(
                               isIsometries: Boolean,
                               levelPath: String,
                               difficulty: LevelDifficulty
                             ) extends JPanel(new BorderLayout()) {

  private var level: Level = LevelOps.buildLevel(levelPath)

  // čuvamo početni nivo za Reset
  private val originalLevel: Level = level
  // poslednja primenjena izometrija (za kvazi-inverz)
  private var appliedIsos: List[Iso] = Nil


  // pick mod za biranje klikom po tabli
  private object PickKind extends Enumeration {
    val NonePickKind, SectorStart, SectorEnd, Center = Value
  }

  import PickKind._

  private var pickMode: PickKind.Value = NonePickKind

  // parametri izometrije
  private var sectorR1 = 0
  private var sectorC1 = 0
  private var sectorR2 = math.max(0, level.rows - 1)
  private var sectorC2 = math.max(0, level.cols - 1)
  private var centerR = level.rows / 2
  private var centerC = level.cols / 2

  private var mergeMode: MergeMode = MergeMode.Opaque
  private var boundaryMode: BoundaryMode = BoundaryMode.Clipping


  private val board: BoardPanel = new BoardPanel(
    onLeftClick = (r, c) => {
      pickMode match {
        case PickKind.SectorStart =>
          sectorR1 = r; sectorC1 = c
          pickMode = PickKind.NonePickKind
          board.setHintAt(None)
          renderFromLevel()

        case PickKind.SectorEnd =>
          sectorR2 = r; sectorC2 = c
          pickMode = PickKind.NonePickKind
          board.setHintAt(None)
          renderFromLevel()

        case PickKind.Center =>
          centerR = r; centerC = c
          pickMode = PickKind.NonePickKind
          board.setHintAt(Some((r, c))) // vizuelni marker centra
          renderFromLevel()

        case PickKind.NonePickKind =>
          // regularno editovanje
          LevelOps.toggleAt(level, r, c).foreach { lv =>
            level = lv
            renderFromLevel()
          }
      }
    }
    ,
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

  private val btnReset = new JButton("Reset")
  private val btnInverse = new JButton("Inverse (quasi)")

  top.add(btnReset)
  top.add(btnInverse)

  // Reset na originalni nivo
  btnReset.addActionListener(_ => {
    level = originalLevel
    appliedIsos = Nil
    board.setHintAt(None)
    pickMode = PickKind.NonePickKind
    renderFromLevel()
  })

  // Kvazi-inverz poslednje izometrije (radi nad trenutnim levelom)
  btnInverse.addActionListener(_ => {
    appliedIsos match {
      case iso :: rest =>
        val inv = iso.inverse
        IsoOps.safeApply(inv, level) match {
          case Right(lv2) =>
            level = lv2
            appliedIsos = rest // uspešan inverse → skini sa steka
            board.setHintAt(None)
            pickMode = PickKind.NonePickKind
            renderFromLevel()
          case Left(errs) =>
            val msg = errs.mkString("• ", "\n• ", "")
            JOptionPane.showMessageDialog(this, msg, "Inverse error", JOptionPane.WARNING_MESSAGE)
        }
      case Nil =>
        java.awt.Toolkit.getDefaultToolkit.beep()
        JOptionPane.showMessageDialog(
          this,
          "No applied isometries to invert.",
          "Inverse",
          JOptionPane.INFORMATION_MESSAGE
        )
    }
  })


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

    // --- Sector ---
    val tfR1 = new JTextField(sectorR1.toString, 3)
    val tfC1 = new JTextField(sectorC1.toString, 3)
    val tfR2 = new JTextField(sectorR2.toString, 3)
    val tfC2 = new JTextField(sectorC2.toString, 3)
    val btnPickStart = new JButton("Pick r1,c1")
    val btnPickEnd = new JButton("Pick r2,c2")
    btnPickStart.addActionListener(_ => {
      pickMode = PickKind.SectorStart; board.setHintAt(None)
    })
    btnPickEnd.addActionListener(_ => {
      pickMode = PickKind.SectorEnd; board.setHintAt(None)
    })

    val sectorPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    sectorPanel.add(new JLabel("Sector r1,c1 – r2,c2:"))
    sectorPanel.add(tfR1);
    sectorPanel.add(tfC1)
    sectorPanel.add(new JLabel("—"));
    sectorPanel.add(tfR2);
    sectorPanel.add(tfC2)
    sectorPanel.add(btnPickStart);
    sectorPanel.add(btnPickEnd)
    p.add(sectorPanel)

    // --- Center ---
    val tfCR = new JTextField(centerR.toString, 3)
    val tfCC = new JTextField(centerC.toString, 3)
    val btnPickCenter = new JButton("Pick center")
    btnPickCenter.addActionListener(_ => {
      pickMode = PickKind.Center
    })
    val centerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    centerPanel.add(new JLabel("Center (r,c):"))
    centerPanel.add(tfCR);
    centerPanel.add(tfCC)
    centerPanel.add(btnPickCenter)
    p.add(centerPanel)

    // --- Modes ---
    val mergeOpaque = new JRadioButton("Opaque", true)
    val mergeTransp = new JRadioButton("Transparent", false)
    val grpMerge = new ButtonGroup();
    grpMerge.add(mergeOpaque);
    grpMerge.add(mergeTransp)

    val boundClip = new JRadioButton("Clipping", true)
    val boundExp = new JRadioButton("Expanding", false)
    val grpBound = new ButtonGroup();
    grpBound.add(boundClip);
    grpBound.add(boundExp)

    val modesPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    modesPanel.add(new JLabel("Merge:"));
    modesPanel.add(mergeOpaque);
    modesPanel.add(mergeTransp)
    modesPanel.add(Box.createHorizontalStrut(12))
    modesPanel.add(new JLabel("Boundary:"));
    modesPanel.add(boundClip);
    modesPanel.add(boundExp)
    p.add(modesPanel)

    // --- Operacije 1: Rotate ---
    val ops1 = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnRotCW = new JButton("Rotate CW 90°")
    val btnRotCCW = new JButton("Rotate CCW 90°")
    ops1.add(btnRotCW);
    ops1.add(btnRotCCW)
    p.add(ops1)

    // --- Operacije 2: Reflect Row/Col ---
    val ops2 = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfRowAxis = new JTextField(3);
    tfRowAxis.setToolTipText("row index")
    val tfColAxis = new JTextField(3);
    tfColAxis.setToolTipText("col index")
    val btnRefRow = new JButton("Reflect Row")
    val btnRefCol = new JButton("Reflect Col")
    ops2.add(new JLabel("Row:"));
    ops2.add(tfRowAxis);
    ops2.add(btnRefRow)
    ops2.add(Box.createHorizontalStrut(6))
    ops2.add(new JLabel("Col:"));
    ops2.add(tfColAxis);
    ops2.add(btnRefCol)
    p.add(ops2)

    // --- Operacije 3: Diagonale ---
    val ops3 = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val btnRefDiagMain = new JButton("Reflect Main diag")
    val btnRefDiagAnti = new JButton("Reflect Anti diag")
    ops3.add(btnRefDiagMain);
    ops3.add(btnRefDiagAnti)
    p.add(ops3)

    // --- Operacije 4: Translate & Central ---
    val ops4 = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfDY = new JTextField(3);
    val tfDX = new JTextField(3)
    val btnTranslate = new JButton("Translate")
    val btnCentral = new JButton("Central symmetry")
    ops4.add(new JLabel("dy:"));
    ops4.add(tfDY)
    ops4.add(new JLabel("dx:"));
    ops4.add(tfDX);
    ops4.add(btnTranslate)
    ops4.add(Box.createHorizontalStrut(6));
    ops4.add(btnCentral)
    p.add(ops4)
    p.add(buildCustomIsosPanel())

    // --- Helper za čitanje parametara ---
    def readParams(): Boolean = {
      def okInt(t: JTextField) = t.getText.trim.nonEmpty && t.getText.trim.forall(ch => ch.isDigit || ch == '-')

      val ok = List(tfR1, tfC1, tfR2, tfC2, tfCR, tfCC).forall(okInt)
      if (!ok) {
        JOptionPane.showMessageDialog(this, "Fill integers for sector and center.", "Isometries", JOptionPane.WARNING_MESSAGE)
        false
      } else {
        sectorR1 = tfR1.getText.trim.toInt;
        sectorC1 = tfC1.getText.trim.toInt
        sectorR2 = tfR2.getText.trim.toInt;
        sectorC2 = tfC2.getText.trim.toInt
        centerR = tfCR.getText.trim.toInt;
        centerC = tfCC.getText.trim.toInt
        mergeMode = if (mergeOpaque.isSelected) MergeMode.Opaque else MergeMode.Transparent
        boundaryMode = if (boundClip.isSelected) BoundaryMode.Clipping else BoundaryMode.Expanding
        true
      }
    }

    // --- Handleri (Apply + Undo push) ---
    btnRotCW.addActionListener(_ => if (readParams()) {
      val iso = LevelIsometries.rotateCW(
        Sector(sectorR1, sectorC1, sectorR2, sectorC2),
        center = (centerR, centerC),
        merge = mergeMode, boundary = boundaryMode
      )
      applyIsoWithUndo(iso)
    })

    btnRotCCW.addActionListener(_ => if (readParams()) {
      val iso = LevelIsometries.rotateCCW(
        Sector(sectorR1, sectorC1, sectorR2, sectorC2),
        center = (centerR, centerC),
        merge = mergeMode, boundary = boundaryMode
      )
      applyIsoWithUndo(iso)
    })

    btnRefRow.addActionListener(_ => if (readParams()) {
      parseInt(tfRowAxis.getText) match {
        case Some(rIdx) =>
          val iso = LevelIsometries.reflectRow(
            Sector(sectorR1, sectorC1, sectorR2, sectorC2),
            rowIndex = rIdx, merge = mergeMode, boundary = boundaryMode
          )
          applyIsoWithUndo(iso)
        case None =>
          JOptionPane.showMessageDialog(this, "Row index must be integer.", "Reflect Row", JOptionPane.WARNING_MESSAGE)
      }
    })

    btnRefCol.addActionListener(_ => if (readParams()) {
      parseInt(tfColAxis.getText) match {
        case Some(cIdx) =>
          val iso = LevelIsometries.reflectCol(
            Sector(sectorR1, sectorC1, sectorR2, sectorC2),
            colIndex = cIdx, merge = mergeMode, boundary = boundaryMode
          )
          applyIsoWithUndo(iso)
        case None =>
          JOptionPane.showMessageDialog(this, "Col index must be integer.", "Reflect Col", JOptionPane.WARNING_MESSAGE)
      }
    })

    btnRefDiagMain.addActionListener(_ => if (readParams()) {
      val iso = LevelIsometries.reflectDiagMain(
        Sector(sectorR1, sectorC1, sectorR2, sectorC2),
        merge = mergeMode, boundary = boundaryMode
      )
      applyIsoWithUndo(iso)
    })

    btnRefDiagAnti.addActionListener(_ => if (readParams()) {
      val iso = LevelIsometries.reflectDiagAnti(
        Sector(sectorR1, sectorC1, sectorR2, sectorC2),
        merge = mergeMode, boundary = boundaryMode
      )
      applyIsoWithUndo(iso)
    })

    btnTranslate.addActionListener(_ => if (readParams()) {
      (parseInt(tfDY.getText), parseInt(tfDX.getText)) match {
        case (Some(dy), Some(dx)) =>
          val iso = LevelIsometries.translate(
            Sector(sectorR1, sectorC1, sectorR2, sectorC2),
            dy = dy, dx = dx, merge = mergeMode, boundary = boundaryMode
          )
          applyIsoWithUndo(iso)
        case _ =>
          JOptionPane.showMessageDialog(this, "dy/dx must be integers.", "Translate", JOptionPane.WARNING_MESSAGE)
      }
    })

    btnCentral.addActionListener(_ => if (readParams()) {
      val iso = LevelIsometries.central(
        Sector(sectorR1, sectorC1, sectorR2, sectorC2),
        center = (centerR, centerC), merge = mergeMode, boundary = boundaryMode
      )
      applyIsoWithUndo(iso)
    })

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

  private def applyIsoWithUndo(iso: Iso): Unit = {
    IsoOps.safeApply(iso, level) match {
      case Right(lv2) =>
        // zapamti primenjeni korak (za chain quasi-inverse)
        appliedIsos = iso :: appliedIsos
        level = lv2
        board.setHintAt(None)
        pickMode = PickKind.NonePickKind
        renderFromLevel()
      case Left(errs) =>
        val msg = errs.mkString("• ", "\n• ", "")
        JOptionPane.showMessageDialog(this, msg, "Isometry error", JOptionPane.WARNING_MESSAGE)
    }
  }

  private def buildCustomIsosPanel(): JComponent = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    p.setBorder(BorderFactory.createTitledBorder("Custom isometries"))

    val files = io.IsometryIO.listFiles()
    if (files.isEmpty) {
      p.add(new JLabel("No files in /isometries"))
    } else {
      files.foreach { fname =>
        io.IsometryIO.load(fname) match {
          case Right(iso) =>
            val btn = new JButton(fname.stripSuffix(".txt"))
            btn.setToolTipText(s"Apply '${fname}'")
            btn.addActionListener(_ => applyIsoWithUndo(iso))
            p.add(btn)
          case Left(err) =>
            val btn = new JButton(fname.stripSuffix(".txt"))
            btn.setEnabled(false)
            btn.setToolTipText(s"Invalid: $err")
            p.add(btn)
        }
      }
    }

    // opcionalno: refresh dugme
    val btnReload = new JButton("Reload")
    btnReload.addActionListener(_ => {
      // rekonstruiši panel naivno:
      val parent = p.getParent
      val idx = parent.asInstanceOf[JComponent].getComponentZOrder(p)
      parent.remove(p)
      parent.add(buildCustomIsosPanel(), idx)
      parent.revalidate()
      parent.repaint()
    })
    p.add(btnReload)

    p
  }


}
