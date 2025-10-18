package gui.widgets

import java.awt.{Color, Font, GridLayout, Insets}
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{BorderFactory, JButton, JPanel, SwingUtilities, UIManager}
import model.{CellContent, CellState, GameState}

final class BoardPanel(
                        onLeftClick: (Int, Int) => Unit,
                        onRightClick: (Int, Int) => Unit
                      ) extends JPanel {

  private var buttons: Vector[Vector[JButton]] = Vector.empty

  private var explodedAt: Option[(Int, Int)] = None
  private var hintAt: Option[(Int, Int)] = None

  private val defaultFg: Color = UIManager.getColor("Button.foreground")
  private val defaultBg: Color = UIManager.getColor("Button.background")
  private val revealedBg: Color = new Color(230, 230, 230)
  private val numberFont: Font = UIManager.getFont("Button.font").deriveFont(Font.BOLD, 20f)

  /** Renders the game board according to the given GameState. */
  def render(gs: GameState): Unit = {
    val rows = gs.board.rows
    val cols = gs.board.cols

    if (buttons.isEmpty || buttons.length != rows || buttons.head.length != cols) {
      removeAll()
      setLayout(new GridLayout(rows, cols, 2, 2))
      buttons = Vector.tabulate(rows, cols) { (r, c) =>
        val b = new JButton()
        b.setFocusPainted(false)
        b.setMargin(new Insets(0, 0, 0, 0))
        b.setFont(numberFont)

        b.addMouseListener(new MouseAdapter {
          override def mousePressed(e: MouseEvent): Unit = {
            if (SwingUtilities.isLeftMouseButton(e)) onLeftClick(r, c)
            if (SwingUtilities.isRightMouseButton(e)) onRightClick(r, c)
          }
        })

        add(b); b
      }
    }

    for (r <- 0 until rows; c <- 0 until cols) {
      val b = buttons(r)(c)

      // reset styles
      b.setForeground(defaultFg)
      b.setBackground(defaultBg)
      b.setOpaque(true)
      b.setEnabled(true)

      gs.state(r)(c) match {
        case CellState.Hidden =>
          b.setText("")

        case CellState.Flagged =>
          b.setText("⚑")
          b.setForeground(Color.red)

        case CellState.Revealed =>
          b.setBackground(revealedBg)
          val cell = gs.board.grid(r)(c)
          cell.content match {
            case CellContent.Mine =>
              b.setText("💣")
              if (explodedAt.contains((r,c))) {
                b.setBackground(new Color(255, 120, 120))
                b.setForeground(Color.black)
              } else {
                b.setBackground(new Color(230,230,230))
                b.setForeground(new Color(178,0,0))
              }

            case CellContent.Clear =>
              val n = cell.adjacentMines
              if (n == 0) {
                b.setText("")
              } else {
                b.setText(n.toString)
                b.setForeground(numberColor(n))
              }
          }
      }

      if (hintAt.contains((r, c))) {
        b.setBackground(new Color(255, 235, 59)) // bright yellow
        b.setOpaque(true)
      }

    }

    revalidate(); repaint()
  }

  /* Set coordinates of exploded mine. */
  def setExplodedAt(rc: Option[(Int, Int)]): Unit = {
    explodedAt = rc
  }

  /* Set coordinates of hinted field. */
  def setHintAt(rc: Option[(Int, Int)]): Unit = { hintAt = rc }

  /* Separate color for each number for better visibility. */
  private def numberColor(n: Int): Color = n match {
    case 1 => Color.blue
    case 2 => new Color(0, 128, 0)
    case 3 => Color.red
    case 4 => new Color(0, 0, 128)
    case 5 => new Color(128, 0, 0)
    case 6 => new Color(0, 128, 128)
    case 7 => Color.black
    case 8 => Color.gray
    case _ => defaultFg
  }
}
