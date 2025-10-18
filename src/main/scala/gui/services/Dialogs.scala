package gui.services

import javax.swing._
import java.awt.Component
import logic.level.LevelDifficulty

object Dialogs {

  sealed trait StartChoice

  case object StartNew extends StartChoice

  case object ResumeOld extends StartChoice

  /** First step: Start new game or Resume old game */
  def chooseStartOrResume(parent: Component): Option[StartChoice] = {
    val options = Array[AnyRef]("Start new game", "Resume old game")
    val icon = UIManager.getIcon("OptionPane.questionIcon")
    val res = JOptionPane.showOptionDialog(
      parent,
      "Choose an option:",
      "Start game",
      JOptionPane.DEFAULT_OPTION,
      JOptionPane.QUESTION_MESSAGE,
      icon,
      options,
      options(0)
    )
    res match {
      case 0 => Some(StartNew)
      case 1 => Some(ResumeOld)
      case _ => None
    }
  }

  /** Second step: difficulty */
  def chooseDifficulty(parent: Component): Option[LevelDifficulty] = {
    val options = Array[AnyRef]("beginner", "intermediate", "expert")
    val icon = UIManager.getIcon("OptionPane.questionIcon")
    val res = JOptionPane.showOptionDialog(
      parent,
      "Choose difficulty:",
      "New game",
      JOptionPane.DEFAULT_OPTION,
      JOptionPane.QUESTION_MESSAGE,
      icon,
      options,
      options(0)
    )
    res match {
      case 0 => Some(LevelDifficulty.Beginner)
      case 1 => Some(LevelDifficulty.Intermediate)
      case 2 => Some(LevelDifficulty.Expert)
      case _ => None
    }
  }

  /** Third step: choose a level (file name) from a combo box */
  def chooseLevel(parent: Component, levels: Seq[String]): Option[String] = {
    val combo = new JComboBox[String](levels.toArray)
    val panel = new JPanel()
    panel.add(new JLabel("Choose level:"))
    panel.add(combo)
    val res = JOptionPane.showConfirmDialog(
      parent,
      panel,
      "Choose level",
      JOptionPane.OK_CANCEL_OPTION,
      JOptionPane.QUESTION_MESSAGE
    )
    if (res == JOptionPane.OK_OPTION) Option(combo.getSelectedItem.asInstanceOf[String])
    else None
  }
}
