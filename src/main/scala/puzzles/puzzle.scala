object Puzzle {
  val EASY = "Easy"
  val MEDIUM = "Medium"
  val HARD = "hard"
  
  //BELOW HELPER METHODS FOR IMPLEMENTATION
  
  /**
   * Turns a board to map of the column index to the column
   * i.e.
   * 123456789 <- column index
   * x1xx2xx3x
   * 2x3xx4xx1
   */
  def boardMapToColumns(puzzle: Vector[Vector[Option[Int]]]): Map[Int, Vector[Option[Int]]] = {
    puzzle.foldLeft(Map.empty[Int, Vector[Option[Int]]]){ case (boardInColumns, row) =>
      row.foldLeft(boardInColumns) { case (board, columnValueOpt) =>
        val cindex = row.indexOf(columnValueOpt)
        val column = board.getOrElse(cindex, Vector.empty[Option[Int]])
        board + (cindex -> (column :+ columnValueOpt))
      }
    }
  }
  
  /**
   * Turns a board to map of the row index to the column
   * i.e.
   * Index
   * 1  xxx1x2x4
   * 2  xx1x3x5x
   * 3  xxx5x2x9
   * 4  ...
   * 5  ...
   */
  def boardMapToRows(puzzle: Vector[Vector[Option[Int]]]): Map[Int, Vector[Option[Int]]] = {
    puzzle.foldLeft(Map.empty[Int, Vector[Option[Int]]]){ case (boardInColumns, row) =>
      val rindex = puzzle.indexOf(row)
      boardInColumns + (rindex -> row)
    }
  }
  
  /**
   * A complete mapping of the index of every row,column to the Option[Int]
   */
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
    val size = puzzle.length
    val sqrt = Math.sqrt(size).toInt
    puzzle.foldLeft(Map.empty[(Int, Int), Vector[Option[Int]]]) { case (squares, row) =>
      val rindex = puzzle.indexOf(row)
      row.foldLeft(squares) { case (s, numberOpt) =>
        val cindex = row.indexOf(numberOpt)
        val square = s.getOrElse((rindex/sqrt, cindex/sqrt), Vector.empty[Option[Int]])
        s + ((rindex/sqrt, cindex/sqrt) -> (square :+ numberOpt))
      }
    }
  }
  
  /**
   * Turns the board into a 2D array
   */
  def boardTo2DArray(puzzle: Vector[Vector[Option[Int]]]): Array[Array[Option[Int]]] = {
    val board = for {
      row <- puzzle
    } yield {
      row.toArray
    }
    board.toArray
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
  
  /**
   * For debugging use
   */
  def printBoard() = {
    println(name)
    difficulty.map(println(_))
    size.map(println(_))
    for {
      line <- board
    } yield {
      for {
        numb <- line
      } yield {
        numb match {
          case Some(number) => print(s":$number:")
          case None => print(":x:")
        }
      }
      println()
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
  private def checkNumberIsWithinInterval(number: Int): Boolean = {
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
  private def validateLine(line: Vector[Option[Int]], valid: Boolean): Boolean = {
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
    validateRows(puzzle) && validateColumns(puzzle) && validateSquares(puzzle) && validateInputNumbers(puzzle)
  }
  
  private def validateRows(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    puzzle.foldLeft(true){ case (valid, row) => 
      validateLine(row, valid)
    }
  }
  
  private def validateColumns(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    val boardMapInColumns = Puzzle.boardMapToColumns(puzzle)
    boardMapInColumns.foldLeft(true) { case (valid, (cindex, column)) =>
      validateLine(column, valid)
    }
  }
  
  private def validateSquares(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    val squares = Puzzle.boardMapToSquares(puzzle)
    squares.foldLeft(true) { case (valid, (cindex, column)) =>
      validateLine(column, valid)
    }
  }
  
  private def validateInputNumbers(puzzle: Vector[Vector[Option[Int]]]): Boolean = {
    val solutionMap = Puzzle.boardMapToRowsAndColumns(puzzle)
    val originalMap = Puzzle.boardMapToRowsAndColumns(board)
    solutionMap.keys.foldLeft(true) { case (valid, (rindex, cindex)) =>
      val originalVal = originalMap.getOrElse((rindex,cindex), None)
      if(originalVal.nonEmpty){
        val solutionVal = solutionMap.getOrElse((rindex,cindex), None)
        if(solutionVal == originalVal){
          valid
        } else {
          false
        }
      } else {
        valid
      }
    }
  }
}
