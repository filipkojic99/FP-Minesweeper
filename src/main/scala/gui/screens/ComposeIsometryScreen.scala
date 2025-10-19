package gui.screens

import java.awt.{BorderLayout, CardLayout, FlowLayout, GridLayout}
import javax.swing._
import javax.swing.border.EmptyBorder
import scala.jdk.CollectionConverters._

import io.IsometryIO

final class ComposeIsometryScreen(onSaved: () => Unit = () => ()) extends JPanel(new BorderLayout()) {

  // ===== Left: steps list =====
  private val stepsModel = new DefaultListModel[String]()
  private val stepsList  = new JList[String](stepsModel)
  stepsList.setVisibleRowCount(12)

  private val btnUp     = new JButton("Up")
  private val btnDown   = new JButton("Down")
  private val btnRemove = new JButton("Remove")
  private val btnClear  = new JButton("Clear all")

  private val leftBtns = new JPanel(new GridLayout(0,1,6,6))
  leftBtns.add(btnUp)
  leftBtns.add(btnDown)
  leftBtns.add(btnRemove)
  leftBtns.add(new JSeparator())
  leftBtns.add(btnClear)

  private val leftPanel = new JPanel(new BorderLayout(8,8))
  leftPanel.setBorder(new EmptyBorder(10,10,10,10))
  leftPanel.add(new JLabel("Steps (top → bottom):"), BorderLayout.NORTH)
  leftPanel.add(new JScrollPane(stepsList), BorderLayout.CENTER)
  leftPanel.add(leftBtns, BorderLayout.EAST)

  // ===== Right: form =====
  private val cbKind = new JComboBox[String](Array(
    "ROTATE90",
    "REFLECT_ROW", "REFLECT_COL", "REFLECT_MAIN", "REFLECT_ANTI",
    "TRANSLATE",
    "CENTRAL"
  ))

  // sector
  private val tfR1 = new JTextField("0", 4)
  private val tfC1 = new JTextField("0", 4)
  private val tfR2 = new JTextField("0", 4)
  private val tfC2 = new JTextField("0", 4)

  // merge/boundary
  private val rbOpaque = new JRadioButton("Opaque", true)
  private val rbTransp = new JRadioButton("Transparent", false)
  private val grpMerge = new ButtonGroup; grpMerge.add(rbOpaque); grpMerge.add(rbTransp)

  private val rbClip = new JRadioButton("Clipping", true)
  private val rbExp  = new JRadioButton("Expanding", false)
  private val grpBound = new ButtonGroup; grpBound.add(rbClip); grpBound.add(rbExp)

  // op-specific cards
  private val cards = new JPanel(new CardLayout())

  // ROTATE90
  private val rotatePanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val cbDir = new JComboBox[String](Array("CW","CCW"))
    cbDir.setName("dir")
    val tfCR = new JTextField("0", 4); tfCR.setName("cr")
    val tfCC = new JTextField("0", 4); tfCC.setName("cc")
    p.add(new JLabel("dir:")); p.add(cbDir)
    p.add(new JLabel("  cr:")); p.add(tfCR)
    p.add(new JLabel("  cc:")); p.add(tfCC)
    p.setName("ROTATE90")
    p
  }

  // REFLECT_ROW
  private val reflectRowPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfR0 = new JTextField("0", 4); tfR0.setName("r0")
    p.add(new JLabel("row r0:")); p.add(tfR0)
    p.setName("REFLECT_ROW")
    p
  }

  // REFLECT_COL
  private val reflectColPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfC0 = new JTextField("0", 4); tfC0.setName("c0")
    p.add(new JLabel("col c0:")); p.add(tfC0)
    p.setName("REFLECT_COL")
    p
  }

  // REFLECT_MAIN
  private val reflectMainPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    p.add(new JLabel("Main diagonal (no extra params)"))
    p.setName("REFLECT_MAIN")
    p
  }

  // REFLECT_ANTI
  private val reflectAntiPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    p.add(new JLabel("Anti diagonal (no extra params)"))
    p.setName("REFLECT_ANTI")
    p
  }

  // TRANSLATE
  private val translatePanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfDY = new JTextField("0", 4); tfDY.setName("dy")
    val tfDX = new JTextField("0", 4); tfDX.setName("dx")
    p.add(new JLabel("dy:")); p.add(tfDY)
    p.add(new JLabel("  dx:")); p.add(tfDX)
    p.setName("TRANSLATE")
    p
  }

  // CENTRAL
  private val centralPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val tfCR = new JTextField("0", 4); tfCR.setName("cr")
    val tfCC = new JTextField("0", 4); tfCC.setName("cc")
    p.add(new JLabel("cr:")); p.add(tfCR)
    p.add(new JLabel("  cc:")); p.add(tfCC)
    p.setName("CENTRAL")
    p
  }

  // register cards
  cards.add(rotatePanel,       "ROTATE90")
  cards.add(reflectRowPanel,   "REFLECT_ROW")
  cards.add(reflectColPanel,   "REFLECT_COL")
  cards.add(reflectMainPanel,  "REFLECT_MAIN")
  cards.add(reflectAntiPanel,  "REFLECT_ANTI")
  cards.add(translatePanel,    "TRANSLATE")
  cards.add(centralPanel,      "CENTRAL")

  private val btnAddStep   = new JButton("Add step")
  private val btnPreview   = new JButton("Preview line")
  private val tfName       = new JTextField(16)
  private val btnSave      = new JButton("Save as…")

  private val rightForm = new JPanel(new GridLayout(0,1,6,6)) {
    setBorder(new EmptyBorder(10,10,10,10))
  }

  private def mkLine(): Option[String] = {
    def txt(tf: JTextField): String = tf.getText.trim
    def asInt(tf: JTextField): Option[Int] =
      if (txt(tf).matches("-?\\d+")) Some(txt(tf).toInt) else None

    // sektor
    val allOk =
      List(tfR1,tfC1,tfR2,tfC2).forall(tf => txt(tf).matches("-?\\d+"))
    if (!allOk) {
      JOptionPane.showMessageDialog(this, "Enter integers for sector r1,c1,r2,c2.", "Compose", JOptionPane.WARNING_MESSAGE)
      return None
    }
    val (r1,c1,r2,c2) = (txt(tfR1),txt(tfC1),txt(tfR2),txt(tfC2))

    val merge = if (rbOpaque.isSelected) "Opaque" else "Transparent"
    val boundary = if (rbClip.isSelected) "Clipping" else "Expanding"

    val kind = cbKind.getSelectedItem.asInstanceOf[String]
    val cardP = cards.getComponents.find(_.getName == kind).get.asInstanceOf[JPanel]

    val mapInputs: Map[String,String] =
      cardP.getComponents.collect { case t: JTextField => t.getName -> t.getText.trim }.toMap ++
        cardP.getComponents.collect { case c: JComboBox[_] => c.getName -> c.getSelectedItem.toString }.toMap

    def needInt(key: String): Int = {
      val s = mapInputs.getOrElse(key, "")
      if (!s.matches("-?\\d+")) throw new IllegalArgumentException(s"Param '$key' must be integer.")
      s.toInt
    }

    try {
      val line: String = kind match {
        case "ROTATE90" =>
          val dir = mapInputs.getOrElse("dir", "CW")
          if (dir != "CW" && dir != "CCW") throw new IllegalArgumentException("dir must be CW or CCW.")
          val cr = needInt("cr"); val cc = needInt("cc")
          s"ROTATE90;r1=$r1;c1=$c1;r2=$r2;c2=$c2;cr=$cr;cc=$cc;dir=$dir;merge=$merge;boundary=$boundary"

        case "REFLECT_ROW" =>
          val r0 = needInt("r0")
          s"REFLECT_ROW;r1=$r1;c1=$c1;r2=$r2;c2=$c2;r0=$r0;merge=$merge;boundary=$boundary"

        case "REFLECT_COL" =>
          val c0 = needInt("c0")
          s"REFLECT_COL;r1=$r1;c1=$c1;r2=$r2;c2=$c2;c0=$c0;merge=$merge;boundary=$boundary"

        case "REFLECT_MAIN" =>
          s"REFLECT_MAIN;r1=$r1;c1=$c1;r2=$r2;c2=$c2;merge=$merge;boundary=$boundary"

        case "REFLECT_ANTI" =>
          s"REFLECT_ANTI;r1=$r1;c1=$c1;r2=$r2;c2=$c2;merge=$merge;boundary=$boundary"

        case "TRANSLATE" =>
          val dy = needInt("dy"); val dx = needInt("dx")
          s"TRANSLATE;r1=$r1;c1=$c1;r2=$r2;c2=$c2;dy=$dy;dx=$dx;merge=$merge;boundary=$boundary"

        case "CENTRAL" =>
          val cr = needInt("cr"); val cc = needInt("cc")
          s"CENTRAL;r1=$r1;c1=$c1;r2=$r2;c2=$c2;cr=$cr;cc=$cc;merge=$merge;boundary=$boundary"
      }
      Some(line)
    } catch {
      case ex: IllegalArgumentException =>
        JOptionPane.showMessageDialog(this, ex.getMessage, "Compose", JOptionPane.WARNING_MESSAGE)
        None
    }
  }

  // assemble right form
  private def buildRight(): JComponent = {
    val topRow = new JPanel(new FlowLayout(FlowLayout.LEFT))
    topRow.add(new JLabel("Operation:"))
    topRow.add(cbKind)

    val sectorRow = new JPanel(new FlowLayout(FlowLayout.LEFT))
    sectorRow.add(new JLabel("Sector r1,c1 — r2,c2:"))
    sectorRow.add(tfR1); sectorRow.add(tfC1)
    sectorRow.add(new JLabel("—"))
    sectorRow.add(tfR2); sectorRow.add(tfC2)

    val modesRow = new JPanel(new FlowLayout(FlowLayout.LEFT))
    modesRow.add(new JLabel("Merge:")); modesRow.add(rbOpaque); modesRow.add(rbTransp)
    modesRow.add(Box.createHorizontalStrut(10))
    modesRow.add(new JLabel("Boundary:")); modesRow.add(rbClip); modesRow.add(rbExp)

    val actionsRow = new JPanel(new FlowLayout(FlowLayout.LEFT))
    actionsRow.add(btnAddStep)
    actionsRow.add(btnPreview)

    val saveRow = new JPanel(new FlowLayout(FlowLayout.LEFT))
    saveRow.add(new JLabel("Save as:"))
    saveRow.add(tfName)
    saveRow.add(new JLabel(".txt"))
    saveRow.add(btnSave)

    rightForm.add(topRow)
    rightForm.add(sectorRow)
    rightForm.add(modesRow)
    rightForm.add(new JSeparator())
    rightForm.add(cards)
    rightForm.add(new JSeparator())
    rightForm.add(actionsRow)
    rightForm.add(saveRow)
    rightForm
  }

  add(leftPanel, BorderLayout.WEST)
  add(buildRight(), BorderLayout.CENTER)

  // ===== behavior =====

  // switch card on kind change
  cbKind.addActionListener(_ => {
    val cl = cards.getLayout.asInstanceOf[CardLayout]
    cl.show(cards, cbKind.getSelectedItem.asInstanceOf[String])
  })

  btnAddStep.addActionListener(_ => {
    mkLine().foreach { line =>
      stepsModel.addElement(line)
      stepsList.setSelectedIndex(stepsModel.size()-1)
    }
  })

  btnPreview.addActionListener(_ => {
    mkLine().foreach { line =>
      JOptionPane.showMessageDialog(this, line, "Preview step", JOptionPane.INFORMATION_MESSAGE)
    }
  })

  btnRemove.addActionListener(_ => {
    val idx = stepsList.getSelectedIndex
    if (idx >= 0) stepsModel.remove(idx)
  })

  btnClear.addActionListener(_ => {
    if (stepsModel.getSize > 0) {
      val ans = JOptionPane.showConfirmDialog(this, "Clear all steps?", "Compose", JOptionPane.OK_CANCEL_OPTION)
      if (ans == JOptionPane.OK_OPTION) stepsModel.clear()
    }
  })

  btnUp.addActionListener(_ => {
    val idx = stepsList.getSelectedIndex
    if (idx > 0) {
      val v = stepsModel.get(idx)
      stepsModel.remove(idx)
      stepsModel.add(idx-1, v)
      stepsList.setSelectedIndex(idx-1)
    }
  })

  btnDown.addActionListener(_ => {
    val idx = stepsList.getSelectedIndex
    if (idx >= 0 && idx < stepsModel.size()-1) {
      val v = stepsModel.get(idx)
      stepsModel.remove(idx)
      stepsModel.add(idx+1, v)
      stepsList.setSelectedIndex(idx+1)
    }
  })

  btnSave.addActionListener(_ => {
    val name = tfName.getText.trim
    if (name.isEmpty) {
      JOptionPane.showMessageDialog(this, "Enter file name (without .txt).", "Save", JOptionPane.WARNING_MESSAGE)
    } else if (stepsModel.isEmpty) {
      JOptionPane.showMessageDialog(this, "No steps to save.", "Save", JOptionPane.WARNING_MESSAGE)
    } else {
      val lines = (0 until stepsModel.size()).map(stepsModel.get).toSeq
      IsometryIO.save(name, lines) match {
        case Right(path) =>
          JOptionPane.showMessageDialog(this, s"Saved to:\n$path", "Save", JOptionPane.INFORMATION_MESSAGE)
          onSaved()
        case Left(err) =>
          JOptionPane.showMessageDialog(this, err, "Save error", JOptionPane.WARNING_MESSAGE)
      }
    }
  })


  // initial card
  {
    val cl = cards.getLayout.asInstanceOf[CardLayout]
    cl.show(cards, "ROTATE90")
  }
}
