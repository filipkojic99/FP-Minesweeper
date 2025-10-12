import cli.Renderer

import scala.io.StdIn.readLine
import io.LevelIO
import model.*
import logic.level.*

object MainLevel {

  def main(args: Array[String]): Unit = {
    var level: Level =
      if (args.nonEmpty) {
        val chars = LevelIO.readLevel(args(0))
        val cells = chars.map(_.map {
          case '#' => CellContent.Mine
          case _ => CellContent.Clear
        })
        Level(cells)
      } else {
        Level(Vector.fill(8, 8)(CellContent.Clear))
      }

    println("Level Playground — type 'help' for commands.")
    Renderer.printLevel(level)

    var running = true
    while (running) {
      val in = Option(readLine("> ")).getOrElse("").trim
      if (in.isEmpty) ()
      else in.split("\\s+").toList match {
        case "show" :: Nil =>
          Renderer.printLevel(level)

        case "addrow" :: where :: Nil =>
          val edge = where.toLowerCase match {
            case "top" => RowEdge.Top
            case "bottom" => RowEdge.Bottom
            case _ => println("Use: addrow top|bottom"); null
          }
          if (edge != null) {
            level = LevelOps.addRow(level, edge, CellContent.Clear)
            Renderer.printLevel(level)
          }

        case "addcol" :: where :: Nil =>
          val edge = where.toLowerCase match {
            case "left" => ColEdge.Left
            case "right" => ColEdge.Right
            case _ => println("Use: addcol left|right"); null
          }
          if (edge != null) {
            level = LevelOps.addCol(level, edge, CellContent.Clear)
            Renderer.printLevel(level)
          }

        case "removerow" :: where :: Nil =>
          val edge = where.toLowerCase match {
            case "top" => RowEdge.Top
            case "bottom" => RowEdge.Bottom
            case _ => println("Use: removerow top|bottom"); null
          }
          if (edge != null) {
            level =
              if (level.rows <= 1) {
                println("Cannot remove last row."); level
              }
              else LevelOps.removeRow(level, edge)
            Renderer.printLevel(level)
          }

        case "removecol" :: where :: Nil =>
          val edge = where.toLowerCase match {
            case "left" => ColEdge.Left
            case "right" => ColEdge.Right
            case _ => println("Use: removecol left|right"); null
          }
          if (edge != null) {
            level =
              if (level.cols <= 1) {
                println("Cannot remove last column."); level
              }
              else LevelOps.removeCol(level, edge)
            Renderer.printLevel(level)
          }

        case "toggle" :: r :: c :: Nil =>
          (r.toIntOption, c.toIntOption) match {
            case (Some(rr), Some(cc)) =>
              LevelOps.toggleAt(level, rr, cc) match {
                case Some(l2) => level = l2; Renderer.printLevel(level)
                case None => println("Out of bounds.")
              }
            case _ => println("Use: toggle r c")
          }

        case "clear" :: r1 :: c1 :: r2 :: c2 :: Nil =>
          (r1.toIntOption, c1.toIntOption, r2.toIntOption, c2.toIntOption) match {
            case (Some(a), Some(b), Some(c), Some(d)) =>
              LevelOps.clearRect(level, a, b, c, d) match {
                case Some(l2) => level = l2; Renderer.printLevel(level)
                case None => println("Rectangle outside level; no-op.")
              }
            case _ => println("Use: clear r1 c1 r2 c2")
          }

        case "validate" :: Nil =>
          LevelValidate.validate(level) match {
            case Right(info) =>
              println(s"VALID ✅  diff=${info.difficulty}  cells=${info.cells}  mines=${info.mines}  allowed=[${info.minMines}..${info.maxMines}]")
            case Left(errs) =>
              println("INVALID ❌  " + errs.mkString(", "))
          }

        case "save" :: diffStr :: Nil =>
          parseDifficulty(diffStr) match {
            case Some(diff) =>
              LevelValidate.validate(level) match {
                case Right(info) =>
                  val path = io.LevelIO.saveLevel(level, diff)
                  println(s"Saved to: $path  (diff=${info.difficulty}, mines=${info.mines})")
                case Left(errs) =>
                  println("Cannot save. Invalid level: " + errs.mkString(", "))
              }
            case None => println("Use: save beginner|intermediate|expert")
          }

        case "help" :: Nil =>
          help()

        case "quit" :: Nil | "exit" :: Nil =>
          running = false

        case other =>
          println(s"Unknown command: ${other.mkString(" ")}  (type 'help')")
      }
    }
  }

  private def parseDifficulty(s: String): Option[LevelDifficulty] =
    s.toLowerCase match {
      case "b" | "beginner" => Some(LevelDifficulty.Beginner)
      case "i" | "intermediate" => Some(LevelDifficulty.Intermediate)
      case "e" | "expert" => Some(LevelDifficulty.Expert)
      case _ => None
    }

  private def help(): Unit = {
    println(
      """Commands:
        |  show
        |  addrow top|bottom
        |  addcol left|right
        |  removerow top|bottom
        |  removecol left|right
        |  toggle r c
        |  clear r1 c1 r2 c2
        |  validate
        |  save beginner|intermediate|expert
        |  help
        |  quit
        |""".stripMargin)
  }
}
