object Puzzle {
  type Grid = Vector[Vector[Cell.CellType]]

  def createGrid(n: Int, m: Int): Grid = {
    Vector.tabulate(n * 2 - 1, m * 2 - 1) { (i, j) =>
      (i % 2, j % 2) match {
        case (0, 0) => '.'
        case (1, 0) => ' '
        case (0, 1) => ' '
        case _ => 'v'
      }
    }
  }

  def gridToString2(grid: Grid): String = {
    grid.map(row => row.map {
      case 'v' => 'v'
      case 'x' => 'x'
      case '.' => '.'
      case other => other
    }.mkString(" ")).mkString("\n")
  }

  def getCellsOfTypes(grid: Grid, targetTypes: List[Cell.CellType] = List()): List[Cell.Cell] = {
    (for {
      i <- 0 until grid.length
      j <- 0 until grid(0).length
      cellType = grid(i)(j)
      if targetTypes.isEmpty || targetTypes.contains(cellType)
    } yield (i, j, cellType)).toList
  }
  
}
