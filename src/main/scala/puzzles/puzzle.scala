object Puzzle {
  val EASY = "Easy"
  val MEDIUM = "Medium"
  val HARD = "hard"
  
  def boardMapToColumns(puzzle: Vector[Vector[Option[Int]]]): Map[Int, Vector[Option[Int]]] = {
    puzzle.foldLeft(Map.empty[Int, Vector[Option[Int]]]){ case (boardInColumns, row) =>
      row.foldLeft(boardInColumns) { case (board, columnValueOpt) =>
        val cindex = row.indexOf(columnValueOpt)
        val column = board.getOrElse(cindex, Vector.empty[Option[Int]])
        board + (cindex -> (column :+ columnValueOpt))
      }
    }
  }
  
  def boardMapToRows(puzzle: Vector[Vector[Option[Int]]]): Map[Int, Vector[Option[Int]]] = {
    puzzle.foldLeft(Map.empty[Int, Vector[Option[Int]]]){ case (boardInColumns, row) =>
      val rindex = puzzle.indexOf(row)
      boardInColumns + (rindex -> row)
    }
  }
  
  def boardMapToRowsAndColumns(puzzle: Vector[Vector[Option[Int]]]): Map[(Int, Int), Option[Int]] = {
    puzzle.foldLeft(Map.empty[(Int,Int), Option[Int]]){ case (map, row) =>
      val rindex = puzzle.indexOf(row)
      row.foldLeft(map) { case (m, numberOpt) =>
        val cindex = puzzle.indexOf(numberOpt)
        map + ((rindex,cindex) -> numberOpt)
      }
    }
  }
  
  /**
   * Maps the board to squares such as
   * (0,0),(0,1),(0,2)
   * (1,0),(1,1),(1,2)
   * (2,0),(2,1),(2,2)
   */
  def boardMapToSquares(puzzle: Vector[Vector[Option[Int]]]): Map[(Int, Int), Vector[Option[Int]]] = {
    puzzle.foldLeft(Map.empty[(Int, Int), Vector[Option[Int]]]) { case (squares, row) =>
      val rindex = puzzle.indexOf(row)
      row.foldLeft(squares) { case (s, numberOpt) =>
        val cindex = row.indexOf(numberOpt)
        val square = s.getOrElse((rindex/3, cindex/3), Vector.empty[Option[Int]])
        s + ((rindex/3, cindex/3) -> (square :+ numberOpt))
      }
    }
  }
}

/**
 * @param name: Name of the puzzle
 * @param difficulty: Difficulty of the puzzle
 * @param size: the size of the board (9x9)
 * @param board: the board we will solve
 */
case class Puzzle(
    name: String,
    difficulty: Option[String] = None,
    size: Option[String] = None,
    board: Vector[Vector[Option[Int]]] = Vector.empty) {
  
  def printBoard() = {
    for {
      line <- board
    } yield {
      println(line)
    }
  }
  
  lazy val sizeFromString: Int = {
    val rowSizeOpt = size map(s => s.split("x")(0))
    val rowSize = rowSizeOpt.getOrElse("9")
    rowSize.toInt
  }
  
  /**
   * Helper method for validating a number is between the
   * allowed interval (i.e 1-9)
   */
  def checkNumberIsWithinInterval(number: Int): Boolean = {
    val numbers = for {
      i <- 1 to sizeFromString
    } yield {
      i
    }
    numbers.contains(number)
  }
  
  /**
   * Makes sure that the line does not contain multiple numbers and that
   * all numbers are between the allowed interval, and that the length
   * is the required length
   */
  def validateLine(line: Vector[Option[Int]], valid: Boolean): Boolean = {
    line.foldLeft(valid){ case (v, numberOpt) =>
        numberOpt match {
          case Some(number) =>
            val count = line.filter { numb => numberOpt == numb }.size
            if(count > 1){
              false
            } else {
              if(checkNumberIsWithinInterval(number)){
                v
              } else {
                false
              }
            }
          case None => false
        }
      } && line.length == sizeFromString
  }
 
  /**
   * Validates that a puzzle is solved correctly
   */
  def validateSolution(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    validateRows(puzzle) && validateColumns(puzzle) && validateSquares(puzzle)
  }
  
  def validateRows(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    puzzle.foldLeft(true){ case (valid, row) => 
      validateLine(row, valid)
    }
  }
  
  def validateColumns(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    val boardMapInColumns = Puzzle.boardMapToColumns(puzzle)
    boardMapInColumns.foldLeft(true) { case (valid, (cindex, column)) =>
      validateLine(column, valid)
    }
  }
  
  def validateSquares(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    val squares = Puzzle.boardMapToSquares(puzzle)
    squares.foldLeft(true) { case (valid, (cindex, column)) =>
      validateLine(column, valid)
    }
  }
}
