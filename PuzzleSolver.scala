import scala.annotation.tailrec
import Rules.solveRules
import Rules.contradictionRules


object PuzzleSolver {

  def isCellSatisfied(grid: Puzzle.Grid, cell: Cell.Cell): Boolean = {
    val (row, col, cellType) = cell
    val lineType = Cell.determineLineType(grid, cell)
    val connectedCells = Cell.getConnectedAdjacentNodes(grid, cell)

    def areConnectedCellsStraight: Boolean = {
      connectedCells.forall { connectedCell =>
        Cell.determineLineType(grid, connectedCell) == Cell.Straight
      }
    }

    def isAtLeastOneConnectedCellCorner: Boolean = {
      connectedCells.exists { connectedCell =>
        Cell.determineLineType(grid, connectedCell) == Cell.Corner
      }
    }

    cellType match {
      case '*' => lineType == Cell.Corner && areConnectedCellsStraight
      case 'o' => (lineType == Cell.Straight || lineType == Cell.Corner) && isAtLeastOneConnectedCellCorner
      case '.' => lineType == Cell.Straight || lineType == Cell.Corner || lineType == Cell.Empty
      case _ => false // this covers other cases, which should not be part of the puzzle
    }
  }

  def isSingleClosedLoop(grid: Puzzle.Grid): Boolean = {
    val allLines = Puzzle.getCellsOfTypes(grid, List('l')).toSet
    var visited = Set[Cell.Cell]()
    var stack = List[Cell.Cell]()

    if (allLines.isEmpty) return false

    val firstLine = allLines.head
    visited += firstLine
    stack = firstLine :: stack

    while (stack.nonEmpty) {
      val currentLine = stack.head
      stack = stack.tail

      val allAdjacentLines = Cell.getConnectedLines(grid, currentLine)
      val unvisitedAdjacentLines = allAdjacentLines.filter(!visited.contains(_))

      if (allAdjacentLines.size != 2) {
        return false  // Not a single closed loop
      }

      visited ++= unvisitedAdjacentLines
      stack = unvisitedAdjacentLines ++ stack
    }

    visited == allLines
  }

  def containsAnyLoop(grid: Puzzle.Grid): (Boolean, Boolean) = {
    val allLines = Puzzle.getCellsOfTypes(grid, List('l')).toSet
    if (allLines.isEmpty || allLines.size == 1) return (false, false)

    var visited = Set[Cell.Cell]()
    var stack = List[Cell.Cell](allLines.head)

    visited += stack.head

    while (stack.nonEmpty) {
      val currentLine = stack.head
      stack = stack.tail

      val allAdjacentLines = Cell.getConnectedLines(grid, currentLine)
      val unvisitedAdjacentLines = allAdjacentLines.filterNot(visited)

      if (allAdjacentLines.size > 1 && allAdjacentLines.forall(visited.contains)) {
        val isSingleClosedLoop = visited == allLines
        return (true, isSingleClosedLoop)
      }

      visited ++= unvisitedAdjacentLines
      stack ++= unvisitedAdjacentLines
    }

    (false, false)
  }

  def isPuzzleComplete(grid: Puzzle.Grid): Boolean = {
    val specialCells = Puzzle.getCellsOfTypes(grid, List('o', '*'))
    val allCellsSatisfied = specialCells.forall(cell => isCellSatisfied(grid, cell))

    allCellsSatisfied && isSingleClosedLoop(grid)
  }

  def applyRule(grid: Puzzle.Grid, cell: Cell.Cell, rule: Rule): Boolean = {
    rule.conditions.forall { condition =>
      val relativeCell = Cell.getRelativeCell(grid, cell, condition.deltaRow, condition.deltaCol)
      relativeCell._3 == condition.cellType
    }
  }

  def solveEdge(grid: Puzzle.Grid, cell: Cell.Cell): Cell.Cell = {
    val (row, col, _) = cell

    val maybeRule = solveRules.find(rule => applyRule(grid, cell, rule))

    maybeRule match {
      case Some(rule) => (row, col, rule.targetCellType)
      case None => (row, col, ' ')
    }
  }

  def isCellLegal(grid: Puzzle.Grid, cell: Cell.Cell): Boolean = {
    val (row, col, cellType) = cell
    val connectedCells = Cell.getConnectedAdjacentNodes(grid, cell)

    def connectedCellCount: Int = connectedCells.size

    // Check for common condition: no more than 2 lines should be connected
    if (connectedCellCount > 2) return false

    // New condition: cell should not be connected to 1 'l' and 3 'x'
    if (connectedCellCount == 1 && Cell.getConnectedXes(grid, cell).size == 3) {
      return false
    }

    cellType match {
      case '*' =>
        // All connected cells should be straight or connected to less than 2 lines
        connectedCells.forall { connectedCell =>
          val adjacentToConnected = Cell.getConnectedAdjacentNodes(grid, connectedCell)
          Cell.determineLineType(grid, connectedCell) == Cell.Straight || adjacentToConnected.size < 2
        }
      case 'o' =>
        // Both of the connected cells can't be straight at the same time
        connectedCells.count(cell => Cell.determineLineType(grid, cell) == Cell.Straight) != 2
      case '.' =>
        // For '.', 2 lines or less connected is okay, so it's always legal based on your rules
        true
      case _ =>
        // This covers other cases, which should not be part of the puzzle
        true
    }
  }

  def areAllCellsLegal(grid: Puzzle.Grid): Boolean = {
    val specialCells = Puzzle.getCellsOfTypes(grid, List('o', '*', '.'))
    val allCellsLegal = specialCells.forall(cell => isCellLegal(grid, cell))

    if (!allCellsLegal) {
      return false
    }

    val (anyLoopExists, isSingleClosedLoop) = containsAnyLoop(grid)

    // If any loop exists, it must be a single closed loop for the puzzle to be legal.
    anyLoopExists == isSingleClosedLoop
  }

  def applyRulesForOneCycle(grid: Puzzle.Grid): (Puzzle.Grid, Boolean) = {
    val emptyEdges = Puzzle.getCellsOfTypes(grid, List(' '))
    var newGrid = grid
    var changed = false

    for (edge <- emptyEdges) {
      val solvedEdge = solveEdge(newGrid, edge)
            if (solvedEdge._3 != ' ') {
        newGrid = Cell.setCell(newGrid, solvedEdge)
        changed = true
      }
    }

    (newGrid, changed)
  }

  def applyRulesUntilStable(grid: Puzzle.Grid): Puzzle.Grid = {
    var currentGrid = grid
    var lastUpdated: Option[Cell.Cell] = None
    var done = false

    // Apply rules until the grid is stable
    while (!done) {
      val (newGrid, changed) = applyRulesForOneCycle(currentGrid)
      currentGrid = newGrid
      if (!changed) {
        done = true
      }
    }

    while (!done) {
      val emptyEdges = Puzzle.getCellsOfTypes(currentGrid, List(' '))

      if (emptyEdges.isEmpty) {
        done = true
      } else {
        var newLastUpdated: Option[Cell.Cell] = None

        for (edge <- emptyEdges) {
          val solvedEdge = solveEdge(currentGrid, edge)
          if (solvedEdge._3 != ' ') {
            currentGrid = Cell.setCell(currentGrid, solvedEdge)
            newLastUpdated = Some(solvedEdge)
          }
        }

        if (newLastUpdated == lastUpdated) {
          // No changes were made, and we are back to the last updated cell.
          done = true
        } else {
          lastUpdated = newLastUpdated
        }
      }
    }

    currentGrid
  }

  def getComplexCellToTest(emptyEdges: List[Cell.Cell], grid: Puzzle.Grid): Option[Cell.Cell] = {
    if (emptyEdges.isEmpty) {
      None
    } else {
      var maxScore = Double.MinValue
      var bestEdge: Option[Cell.Cell] = None

      for (edge <- emptyEdges) {
        var edgeScore = 0.0  // Initialize the score for this edge

        // Use getTouchingCells to get the adjacent cells to this edge
        val touchingCells = Cell.getTouchingCells(grid, edge)

        for (touchingCell <- touchingCells) {
          val (_, _, cellType) = touchingCell
          cellType match {
            case '*' => edgeScore += 2
            case 'o' => edgeScore += 0.9
            case _   => // Do nothing
          }

          // Get the lines connected to this touching cell and accumulate their scores
          val connectedLines = Cell.getConnectedLines(grid, touchingCell)

          for (connectedLine <- connectedLines) {
            connectedLine._3 match {
              case 'l' => edgeScore += 0.4  // Add 0.9 points for 'l'
              case 'x' => edgeScore += 0.02  // Add 0.8 points for 'x'
              case _   => // Do nothing
            }
          }
        }

        // Update maxScore and bestEdge if this edge has a higher score
        if (edgeScore > maxScore) {
          maxScore = edgeScore
          bestEdge = Some(edge)
        }
      }

      bestEdge  // Return the edge with the highest score
    }
  }

  def solvePuzzle(grid: Puzzle.Grid): Option[Puzzle.Grid] = {
    val stableGrid = applyRulesUntilStable(grid)

    // Check if all cells are legal in the stable grid
    if (!areAllCellsLegal(stableGrid)) {
      return None
    }

    if (isPuzzleComplete(stableGrid)) {
      Some(stableGrid)
    } else {
      val emptyEdges = Puzzle.getCellsOfTypes(stableGrid, List(' '))

      getComplexCellToTest(emptyEdges, stableGrid) match {
        case Some(cellToTest) =>
          val (row, col, _) = cellToTest

          // Hypothetical grid with a line ('l') in the cell to test
          val withGuess = Cell.setCell(stableGrid, (row, col, 'l'))

          // Try solving with the hypothetical line
          solvePuzzle(withGuess) match {
            case Some(solution) => return Some(solution)
            case None => ()
          }

          // The hypothetical line led to a contradiction or was illegal, so place an 'x' instead
          val withX = Cell.setCell(stableGrid, (row, col, 'x'))
          solvePuzzle(withX)

        case None => None
      }
    }
  }


  def main(args: Array[String]): Unit = {

    // val totalStartTime = System.nanoTime()  // Record the start time for everything

    // Reading from File
    // val readStartTime = System.nanoTime()
    val inputFilePath = if (args.length > 0) args(0) else "puzzles.txt"
    val grids = PuzzleReaderWriter.readPuzzlesFromFile(inputFilePath)
    // val readEndTime = System.nanoTime()
    // val readTimeElapsed = (readEndTime - readStartTime) / 1e6  // Time in milliseconds

    // Solving Puzzles
    // val solveStartTime = System.nanoTime()
    var solvedPuzzles = List[Puzzle.Grid]()
    for ((grid, index) <- grids.zipWithIndex) {
      solvePuzzle(grid) match {
        case Some(solvedPuzzle) =>
          val rows = (solvedPuzzle.length + 1) / 2
          val cols = (solvedPuzzle(0).length + 1) / 2
          println(s"Solved puzzle ${index + 1} of size ${rows}x${cols}")  // Print the size of the solved grid
          solvedPuzzles = solvedPuzzles :+ solvedPuzzle  // Add solved puzzle to the list
        case None =>
          println("Puzzle is unsolvable")
      }
    }
    // println(s"Time taken to read from file: $readTimeElapsed ms")

    // val solveEndTime = System.nanoTime()
    // val solveTimeElapsed = (solveEndTime - solveStartTime) / 1e6  // Time in milliseconds
    // println(s"Time taken to solve puzzles: $solveTimeElapsed ms")

    // Writing to File
    // val writeStartTime = System.nanoTime()
    val outputFilePath = if (args.length > 1) args(1) else "solved_puzzles.txt"
    PuzzleReaderWriter.writePuzzlesToFile(outputFilePath, solvedPuzzles)
    // val writeEndTime = System.nanoTime()
    // val writeTimeElapsed = (writeEndTime - writeStartTime) / 1e6  // Time in milliseconds
    // println(s"Time taken to write to file: $writeTimeElapsed ms")

    // Total Time
    // val totalEndTime = System.nanoTime()
    // val totalTimeElapsed = (totalEndTime - totalStartTime) / 1e6  // Time in milliseconds
    // println(s"Total time taken: $totalTimeElapsed ms")
  }

}