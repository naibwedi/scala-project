import scala.io.Source
import java.nio.file.{Files, Paths}

case class RuleCondition(deltaRow: Int, deltaCol: Int, cellType: Cell.CellType) {
  def toScalaCode: String = s"RuleCondition($deltaRow, $deltaCol, ${cellType.toString})"
}

case class Rule(targetCellType: Cell.CellType, conditions: List[RuleCondition]) {
  def toScalaCode: String = {
    val conditionsCode = conditions.map(_.toScalaCode).mkString(", ")
    s"Rule(${targetCellType.toString}, List($conditionsCode))"
  }
}

object RuleParser {

  def splitIntoBlocks(lines: List[String]): List[List[String]] = {
    var blocks: List[List[String]] = List()
    var currentBlock: List[String] = List()

    for (line <- lines) {
      if (line.startsWith("#")) {
        if (currentBlock.nonEmpty) {
          blocks = blocks :+ currentBlock
          currentBlock = List()
        }
      } else {
        currentBlock = currentBlock :+ line
      }
    }

    if (currentBlock.nonEmpty) {
      blocks = blocks :+ currentBlock
    }

    blocks
  }

  def parseBlock(block: List[String]): Rule = {
    var targetCellType = ' '
    var targetCellRow = 0
    var targetCellCol = 0

    // First, find the target cell
    for {
      (row, i) <- block.zipWithIndex
      (cell, j) <- row.zipWithIndex
    } {
      if (cell == 'X') {
        targetCellType = 'x'
        targetCellRow = i
        targetCellCol = j
      } else if (cell == 'L') {
        targetCellType = 'l'
        targetCellRow = i
        targetCellCol = j
      }
    }

    // Then, collect the conditions relative to the target cell
    val conditions = for {
      (row, i) <- block.zipWithIndex
      (cell, j) <- row.zipWithIndex if cell != ' '
      if cell != ' ' && !(i == targetCellRow && j == targetCellCol)
    } yield {
      val deltaRow = i - targetCellRow
      val deltaCol = j - targetCellCol
      RuleCondition(deltaRow, deltaCol, cell)
    }

    Rule(targetCellType, conditions.toList)
  }

  def parseFile(filename: String): List[Rule] = {
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    val blocks = splitIntoBlocks(lines)
    blocks.map(parseBlock)
  }

  def rotate90(condition: RuleCondition): RuleCondition = {
    RuleCondition(-condition.deltaCol, condition.deltaRow, condition.cellType)
  }

  def flipHorizontally(condition: RuleCondition): RuleCondition = {
    RuleCondition(condition.deltaRow, -condition.deltaCol, condition.cellType)
  }

  def canonicalForm(conditions: List[RuleCondition]): List[RuleCondition] = {
    conditions.sortBy(c => (c.deltaRow, c.deltaCol))
  }

  def generateRuleVariants(rule: Rule): List[Rule] = {
    val originalConditions = rule.conditions

    val rotated90 = originalConditions.map(rotate90)
    val rotated180 = rotated90.map(rotate90)
    val rotated270 = rotated180.map(rotate90)

    val flippedH = originalConditions.map(flipHorizontally)
    val flippedH_rotated90 = flippedH.map(rotate90)
    val flippedH_rotated180 = flippedH_rotated90.map(rotate90)
    val flippedH_rotated270 = flippedH_rotated180.map(rotate90)

    val allVariants = List(
      originalConditions,
      rotated90,
      rotated180,
      rotated270,
      flippedH,
      flippedH_rotated90,
      flippedH_rotated180,
      flippedH_rotated270
    ).map(canonicalForm)

    // Eliminate duplicates
    val uniqueVariants = allVariants.distinct
    uniqueVariants.map(conditions => Rule(rule.targetCellType, conditions))
  }

  def getAllRulesFromFile(filePath: String): List[Rule] = {
    val rules = parseFile(filePath)
    return rules.flatMap(generateRuleVariants)
  }

  def printRule(rule: Rule): Unit = {

    // Find dimensions of grid needed to display this rule
    val minRow = (0).min(rule.conditions.map(_.deltaRow).min)
    val maxRow = (0).max(rule.conditions.map(_.deltaRow).max)
    val minCol = (0).min(rule.conditions.map(_.deltaCol).min)
    val maxCol = (0).max(rule.conditions.map(_.deltaCol).max)

    // Initialize grid with empty spaces
    val rows = maxRow - minRow + 1
    val cols = maxCol - minCol + 1
    val grid = Array.ofDim[Char](rows, cols)
    for (i <- 0 until rows; j <- 0 until cols) {
      grid(i)(j) = ' '
    }

    // Place the conditions in the grid
    for (condition <- rule.conditions) {
      val row = condition.deltaRow - minRow
      val col = condition.deltaCol - minCol
      grid(row)(col) = condition.cellType
    }

    // Capitalize the target cell to make it stand out
    val targetRow = -minRow
    val targetCol = -minCol

    grid(targetRow)(targetCol) = rule.targetCellType.toUpper

    // Print the grid
    for (row <- grid) {
      println(row.mkString(""))
    }
  }

  def main(args: Array[String]): Unit = {
    // Use sample rule files for demonstration; replace with your actual file paths
    val solveRuleFilePath = "rules.txt"
    val contradictionRuleFilePath = "contradiction_rules.txt"

    // Parse the rules using your existing parser
    val parsedSolveRules = RuleParser.getAllRulesFromFile(solveRuleFilePath)
    val parsedContradictionRules = RuleParser.getAllRulesFromFile(contradictionRuleFilePath)

    def rulesToScalaCode(rules: List[Rule]): String = {
      rules.map(rule => {
        val targetCellType = rule.targetCellType match {
          case '*' => "star"
          case c => c.toString
        }
        val conditionsAsScalaCode = rule.conditions.map { condition =>
          val cellType = condition.cellType match {
            case '*' => "star"
            case c => c.toString
          }
          s"RuleCondition(${condition.deltaRow}, ${condition.deltaCol}, $cellType)"
        }.mkString(", ")

        s"Rule($targetCellType, List($conditionsAsScalaCode))"
      }).mkString(",\n  ")
    }

    val solveRulesAsScalaCode = rulesToScalaCode(parsedSolveRules)
    val contradictionRulesAsScalaCode = rulesToScalaCode(parsedContradictionRules)

    // Generate the content for the new Rules.scala file
    val scalaCodeContent =
      s"""import Cell.CellType
        |
        |object Rules {
        |  val l: Cell.CellType = 'l'
        |  val x: Cell.CellType = 'x'
        |  val o: Cell.CellType = 'o'
        |  val v: Cell.CellType = 'v'
        |  val star: Cell.CellType = '*'
        |
        |  val solveRules: List[Rule] = List(
        |  $solveRulesAsScalaCode
        |  )
        |
        |  val contradictionRules: List[Rule] = List(
        |  $contradictionRulesAsScalaCode
        |  )
        |}""".stripMargin

    // Write the Scala code to a new file called Rules.scala
    Files.write(Paths.get("Rules.scala"), scalaCodeContent.getBytes)
  }

}
