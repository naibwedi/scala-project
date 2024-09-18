object Cell {
  type CellType = Char
  type Cell = (Int, Int, CellType)  // (Row, Column, Type)

  // Define types for line
  sealed trait LineType
  case object Straight extends LineType
  case object Corner extends LineType
  case object End extends LineType
  case object Empty extends LineType
  case object Illegal extends LineType

  def determineLineType(grid: Puzzle.Grid, cell: Cell): LineType = {
    // Extract adjacent cells for readability
    val cell_up = getRelativeCell(grid, cell, 0, 1)._3
    val cell_left = getRelativeCell(grid, cell, -1, 0)._3
    val cell_down = getRelativeCell(grid, cell, 0, -1)._3
    val cell_right = getRelativeCell(grid, cell, 1, 0)._3

    val num_adjacent = List(cell_up, cell_left, cell_down, cell_right).count(_ == 'l')

    num_adjacent match {
      case 0 => Empty
      case 1 => End
      case 2 =>
        if (cell_up == 'l' && cell_down == 'l') {
          Straight
        } else if (cell_left == 'l' && cell_right == 'l') {
          Straight
        } else {
          Corner
        }
      case _ => Illegal
    }
  }

  def getTouchingCells(grid: Puzzle.Grid, cell: Cell): List[Cell] = {
    val (row, col, cellType) = cell

    // Determine if the edge is vertical or horizontal based on its coordinates
    if (row % 2 == 0 && col % 2 == 1) {
      // Horizontal edge, nodes are to the left and right
      List(getRelativeCell(grid, cell, 0, -1), getRelativeCell(grid, cell, 0, 1))
    } else if (row % 2 == 1 && col % 2 == 0) {
      // Vertical edge, nodes are above and below
      List(getRelativeCell(grid, cell, -1, 0), getRelativeCell(grid, cell, 1, 0))
    } else if (row % 2 == 0 && col % 2 == 0) {
      // This is a cell
      List(getRelativeCell(grid, cell, -1, 0), getRelativeCell(grid, cell, 1, 0),
           getRelativeCell(grid, cell, 0, -1), getRelativeCell(grid, cell, 0, 1))
    }
    else {
      // Not an valid square, return an empty list
      List()
    }
  }

  def getConnectedAdjacentNodes(grid: Puzzle.Grid, cell: Cell): List[Cell] = {
    // Define offsets to find adjacent cells
    val offsets = List((-1, 0), (0, -1), (1, 0), (0, 1))
    
    // Create a list to hold connected adjacent nodes
    var connectedAdjacentNodes: List[Cell] = List()

    // Loop through each offset to find adjacent cells
    for ((deltaRow, deltaCol) <- offsets) {
      val adjacentCell = getRelativeCell(grid, cell, deltaRow, deltaCol)
      val (adjRow, adjCol, adjType) = adjacentCell
      
      // Check if the adjacent cell is a line ('l')
      if (adjType == 'l') {
        // Find the node that this line points to
        val connectedNode = getRelativeCell(grid, adjacentCell, deltaRow, deltaCol)
        
        // Add this node to the list of connected adjacent nodes
        connectedAdjacentNodes = connectedNode :: connectedAdjacentNodes
      }
    }
    
    connectedAdjacentNodes
  }

  def getConnectedLines(grid: Puzzle.Grid, cell: Cell): List[Cell] = {
    val (row, col, cellType) = cell
    
    // Check if the cell is a line ('l')
    if (cellType != 'l') return List()
    
    // Define offsets for horizontal and vertical lines
    val horizontalOffsets = List((-2, 0), (-1, 1), (-1, -1), (2, 0), (1, 1), (1, -1))
    val verticalOffsets = List((0, -2), (-1, -1), (1, -1), (0, 2), (-1, 1), (1, 1))
    
    // Determine if the line is horizontal or vertical
    val isHorizontal = (row % 2 == 1) && (col % 2 == 0)
    val isVertical = (row % 2 == 0) && (col % 2 == 1)
    
    // Initialize an empty list to hold the connected lines
    var connectedLines: List[Cell] = List()
    
    // Use the appropriate offsets based on the line's orientation
    val offsets = if (isHorizontal) horizontalOffsets else if (isVertical) verticalOffsets else List()
    
    // Loop through each offset to find connected lines
    for ((deltaRow, deltaCol) <- offsets) {
      val adjacentCell = getRelativeCell(grid, cell, deltaRow, deltaCol)
      val (_, _, adjType) = adjacentCell
      
      // Check if the adjacent cell is a line ('l')
      if (adjType == 'l') {
        connectedLines = adjacentCell :: connectedLines
      }
    }
    
    connectedLines
  }

  def getConnectedXes(grid: Puzzle.Grid, cell: Cell): List[Cell] = {
    val (row, col, cellType) = cell

    // Define offsets to find adjacent cells (edges)
    val offsets = List(
      (-1, 0), // above
      (1, 0),  // below
      (0, -1), // left
      (0, 1)   // right
    )

    // Create a list to hold connected 'x'-es
    var connectedXes: List[Cell] = List()

    // Loop through each offset to find adjacent cells
    for ((deltaRow, deltaCol) <- offsets) {
      val adjacentCell = Cell.getRelativeCell(grid, cell, deltaRow, deltaCol)
      val (_, _, adjType) = adjacentCell

      // Check if the adjacent cell is an 'x'
      if (adjType == 'x') {
        connectedXes = adjacentCell :: connectedXes
      }
    }

    connectedXes
  }

  def getCell(grid: Puzzle.Grid, row: Int, col: Int): Cell = {
    val cellType = 
      if (row >= 0 && row < grid.length && col >= 0 && col < grid(0).length) {
        grid(row)(col)
      } else if (row % 2 != 0 && col % 2 != 0) {
        'v'
      } else {
        'x'
      }
    (row, col, cellType)
  }

  def getRelativeCell(grid: Puzzle.Grid, cell: Cell, deltaRow: Int, deltaCol: Int): Cell = {
    val (row, col, _) = cell
    getCell(grid, row + deltaRow, col + deltaCol)
  }

  def setCell(grid: Puzzle.Grid, cell: Cell): Puzzle.Grid = {
    val (row, col, newCellType) = cell
    if (row >= 0 && row < grid.length && col >= 0 && col < grid(0).length) {
      grid.updated(row, grid(row).updated(col, newCellType))
    } else {
      grid
    }
  }
}
