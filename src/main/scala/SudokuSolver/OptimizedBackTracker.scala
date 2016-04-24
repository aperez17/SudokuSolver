object OptimizedBackTracker extends SudokuSolver {
  val NOT_FOUND = (-1, -1)
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]] = {
    val asTrack = toTracker(puzzle.board)
    val cleansedTrack = removePossibilities(asTrack)
    BackTracker.solve(puzzle)
  }
  
  def backTrack(board: Seq[Seq[Tracker]], rIndex: Int, cIndex: Int): (Seq[Seq[Tracker]], Boolean) = {
    val (newRow, newColumn) = findUnassignedLocation(board, 0, 0)
    if((newRow, newColumn) == (-1,-1)){
      (board, true)
    } else {
      val cell = board(newRow)(newColumn)
      
      for (i <- cell.possiblities){
        val newBoard = addNumberToBoard(board, newRow, newColumn, i)
        val (result, isDone) = backTrack(newBoard, newRow, newColumn)
        if(isDone){
          return (result, true)
        }
      }
      (board, false)
    }
  }
  
  def addNumberToBoard(board: Seq[Seq[Tracker]], rIndex: Int, cIndex: Int, number: Int): Seq[Seq[Tracker]] = {
    val updatedBoard = board.updated(rIndex, board(rIndex).updated(cIndex, Tracker(List.empty[Int], Some(number))))
    updateColumn(updateRow(updatedBoard, rIndex, number), cIndex, number)
  }
  
  def updateRow(board: Seq[Seq[Tracker]], rIndex: Int, number: Int): Seq[Seq[Tracker]] = {
    val newRow = updatePossibilities(board(rIndex), number)
    board.updated(rIndex, newRow)
  }
  
  def updateColumn(board: Seq[Seq[Tracker]], cIndex:Int, number: Int): Seq[Seq[Tracker]] = {
    val asColumns = board.transpose
    val column = asColumns(cIndex)
    val newColumn = updatePossibilities(column, number)
    asColumns.updated(cIndex, newColumn)
  }
  
  def updatePossibilities(line: Seq[Tracker], number: Int): Seq[Tracker] = {
    line.map(track => {
      val possibilities = track.possiblities.filter(_ != number)
      track.copy(possiblities = possibilities)
    })
  }
  
  def updateSquares(board: Seq[Seq[Tracker]], rIndex:Int, cIndex:Int, number:Int): Seq[Seq[Tracker]] = {
    val asSquares = toSquares(board)
   // val squareIndex = findIndex(squares, rIndex, cIndex, number)
    board
  }
  
  def finedIndex(squares: Seq[Tracker], rIndex: Int, cIndex: Int, number: Int): Int = {
    rIndex/3
  }
  
  def findUnassignedLocation(board: Seq[Seq[Tracker]], rIndex: Int, cIndex: Int): (Int, Int) = {
    val size = board.size
    board(rIndex)(cIndex).value match {
      case Some(number) if(cIndex < size-1)  => findUnassignedLocation(board, rIndex, cIndex+1)
      case Some(number) if(rIndex < size-1)  => findUnassignedLocation(board, rIndex+1, 0)
      case Some(number) if(cIndex == size-1 && rIndex == size-1)=> NOT_FOUND
      case None => (rIndex, cIndex)
    }
  }
  
  def removePossibilities(board: Seq[Seq[Tracker]]): Seq[Seq[Tracker]] = {
    val removedColumnCands = eliminateColumnCandidates(board)
    val removedSquareCandidates = eliminateSquareCandidates(removedColumnCands)
    removedSquareCandidates
  }
  
  def toTracker(board: Seq[Seq[Option[Int]]]) = {
    board.map(row => {
      val possiblities = getPossibleCandidates(row)
      row.map(numbOpt => {
        numbOpt match {
          case Some(number) => Tracker(List(number), numbOpt)
          case None => Tracker(possiblities)
        }
      })
    })
  }
  
  /**
   * Eliminates the possible candidates by checking row
   */
  def getPossibleCandidates(row: Seq[Option[Int]]): List[Int] = {
    val knownNumbers = row.flatten
    val list = (1 to row.size).toList
    list.diff(knownNumbers)
  }
  
  /**
   * Eliminates the possible candidates by checking column
   */
  def eliminateColumnCandidates(board: Seq[Seq[Tracker]]): Seq[Seq[Tracker]] = {
    board.transpose.map(eliminateCandidates).transpose
  }
  
  /**
   * Eliminates possible candidates by checking square
   */
  def eliminateSquareCandidates(board: Seq[Seq[Tracker]]) = {
    val boardAsSquares = toSquares(board)
    val eliminatedCandidates = boardAsSquares.map(eliminateCandidates)
    fromSquares(eliminatedCandidates)
  }
  
  /**
   * Divides the board up by squares
   * Too inefficient/time consuming to find immutable solution
   */
  def toSquares(board: Seq[Seq[Tracker]]): Seq[Seq[Tracker]] = {
    val size = board.size
    val n = Math.sqrt(size).toInt
    (for {
      squareX <- 0 to (n - 1)
      squareY <- 0 to (n - 1)
      xOffset = squareX * n
      yOffset = squareY * n
    } yield {
      (for {
        yPos <- (0 + yOffset) to ((n + yOffset) - 1)
        xPos <- (0 + xOffset) to ((n + xOffset) - 1)
        list <- board.lift(yPos)
        number <- list.lift(xPos)
      } yield {
        number
      }).toSeq
    }).toSeq
  }
  
  /**
   * Brings a sequence of squares back to
   * their original state
   */
  def fromSquares(squares: Seq[Seq[Tracker]]): Seq[Seq[Tracker]] = {
    val size = squares.size
    val n = Math.sqrt(size).toInt
    (for {
      squareY <- 0 to (n - 1)
      squareX <- 0 to (n - 1)
      xOffset = squareX * n
      yOffset = squareY
    } yield {
      (for {
        yPos <- (0 + yOffset) to (size - 1) by 3
        xPos <- (0 + xOffset) to ((n + xOffset) - 1)
        list <- squares.lift(yPos)
        number <- list.lift(xPos)
      } yield {
        number
      }).toSeq
    }).toSeq
  }
  
  /**
   * Eliminate candidates from line
   */
  def eliminateCandidates(line: Seq[Tracker]): Seq[Tracker] = {
    val knownNumbers = line.map(_.value).flatten
    val list = (1 to line.size).toList
    line.map(tracker => {
      tracker.possiblities match {
        case List(number) => tracker
        case poss => 
          val eliminatePossiblities = poss.filter { list.contains(_) }
          tracker.copy(possiblities = eliminatePossiblities)
      }
    })
  }
}

case class Tracker(possiblities: List[Int], value: Option[Int] = None)