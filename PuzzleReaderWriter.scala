import scala.io.Source
import scala.util.control.Breaks._
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.ListBuffer

object PuzzleReaderWriter {

  def writePuzzlesToFile(filePath: String, puzzles: List[Puzzle.Grid]): Unit = {
    val writer = new PrintWriter(new File(filePath))
    val sb = new StringBuilder(s"puzzles ${puzzles.size}\n")
    
    puzzles.foreach { grid =>
      val rows = (grid.length + 1) / 2
      val cols = (grid(0).length + 1) / 2
      sb.append(s"size ${cols}x${rows}\n")
      sb.append(gridToString(grid))
      sb.append("\n")
    }
    
    writer.write(sb.toString())
    writer.close()
  }

  def stringToGridCell(cell: Char): Char = cell match {
    case '*' => '*'
    case 'o' => 'o'
    case '_' => '.'
    case x => 
      println(s"Unexpected cell character: '$x'")
      throw new Exception("Unknown cell type")
  }

  def readPuzzlesFromFile(filePath: String): List[Puzzle.Grid] = {
    // Read the entire file content into a single string
    val fileContent = scala.io.Source.fromFile(filePath).mkString

    // Create an iterator from the lines of the file content
    val lines = fileContent.split("\n").iterator

    val puzzles = ListBuffer[Puzzle.Grid]()

    lines.next()  // Skip the first line

    while (lines.hasNext) {
      val sizeLine = lines.next()
      val splitSize = sizeLine.split(" ")(1).split("x")
      val Array(cols, rows) = splitSize.map(_.trim.replaceAll("\"", "").toInt)

      var grid = Puzzle.createGrid(rows, cols)
      var incompleteGrid = false

      for (r <- 0 until rows if !incompleteGrid) {
        if (lines.hasNext) {
          val rowLine = lines.next()
          var c = 0
          for (cell <- rowLine if !cell.isWhitespace) {  // Skip whitespaces directly
            grid = Cell.setCell(grid, (r * 2, c * 2, stringToGridCell(cell)))
            c += 1
          }
        } else {
          println(s"Warning: Not enough lines to read a full ${rows}x${cols} grid.")
          incompleteGrid = true
        }
      }

      if (!incompleteGrid) {
        puzzles += grid
      }
    }

    puzzles.toList
  }

  def getSymbolFromCell(grid: Puzzle.Grid, cell: Cell.Cell): Char = {
    val (rowIndex, colIndex, cellType) = cell

    // Get the adjacent cells
    val adjacentCells = List(
      Cell.getRelativeCell(grid, cell, 0, 1)._3,
      Cell.getRelativeCell(grid, cell, 1, 0)._3,
      Cell.getRelativeCell(grid, cell, 0, -1)._3,
      Cell.getRelativeCell(grid, cell, -1, 0)._3
    )

    // Pattern match to find the correct symbol
    adjacentCells match {
      case List(a, b, c, d) if List(a, b, c, d).count(_ == 'l') == 2 && cellType == '*' => '┼'
      case List(_, a, _, b) if a == 'l' && b == 'l' && cellType == 'o' => '╡'
      case List(a, _, b, _) if a == 'l' && b == 'l' && cellType == 'o' => '╨'
      case List(a, b, _, _) if a == 'l' && b == 'l' => '┌'
      case List(_, a, b, _) if a == 'l' && b == 'l' => '┐'
      case List(_, _, a, b) if a == 'l' && b == 'l' => '┘'
      case List(a, _, _, b) if a == 'l' && b == 'l' => '└'
      case List(a, b, c, _) if a == 'l' && b == 'l' && c == 'l' => '├'
      case List(_, a, b, c) if a == 'l' && b == 'l' && c == 'l' => '┤'
      case List(a, _, b, c) if a == 'l' && b == 'l' && c == 'l' => '┴'
      case List(a, b, _, c) if a == 'l' && b == 'l' && c == 'l' => '┬'
      case List(a, b, c, d) if a == 'l' && b == 'l' && c == 'l' && d == 'l' => '┼'
      case List(a, _, b, _) if a == 'l' && b == 'l' => '─'
      case List(_, a, _, b) if a == 'l' && b == 'l' => '│'
      case _ if cellType == '*' => '*'
      case _ if cellType == 'o' => 'o'
      case _ => ' '
    }
  }

  def gridToString(grid: Puzzle.Grid): String = {
    val sb = new StringBuilder()
    
    for (rowIndex <- 0 until grid.length by 2) {
      val row = grid(rowIndex)
      
      for (colIndex <- 0 until row.length by 2) {
        val cell = row(colIndex)
        sb.append(getSymbolFromCell(grid, (rowIndex, colIndex, cell)))
      }
      
      sb.append("\n")
    }
    
    sb.toString()
  }

}
