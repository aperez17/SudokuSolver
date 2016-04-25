object BackTracker extends SudokuSolver {
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]] = {
    val (newBoard, value) = solveBackTrack(puzzle.board, 0, 0, puzzle.board.size)
    val newPuzzle = puzzle.copy(board = newBoard)
    if(value == false){
      puzzle.board
    } else {
      newBoard
    }
  }
  
  def solveBackTrack(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, size: Int): (Seq[Seq[Option[Int]]], Boolean) = {
    val (newRow, newColumn) = findUnassignedLocation(board, 0, 0, size)
    if((newRow, newColumn) == (-1,-1)){
      (board, true)
    } else {
      val allNumbs = (for {
        i <- 1 to board.size
      } yield {
        i
      })
      
      for (i <- allNumbs){
        if(isSafe(board, newRow, newColumn, i, size)){
          val newBoard = addNumberToBoard(board, newRow, newColumn, i)
          val (result, isDone) = solveBackTrack(newBoard, newRow, newColumn, size)
          if(isDone){
            return (result, true)
          }
        }
      }
      (board, false)
    }
  }
  
  def addNumberToBoard(board :Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int): Seq[Seq[Option[Int]]] = {
    val row = board(rIndex)
    val newRow = row.updated(cIndex, Some(number))
    board.updated(rIndex, newRow)
  }
  
  def findUnassignedLocation(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, size: Int): (Int, Int) = {
    var c = cIndex
    for(i <- rIndex to size-1){
      for(j <- c to size-1){
        if(board(i)(j).isEmpty){
          return (i, j)
        }
      }
      c = 0
    }
    (-1, -1)
  }
  
  
  def usedInRow(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int, size: Int): Boolean = {
    val row = board(rIndex)
    for(i <- 0 to size-1){
      if(row(i) == Some(number)){
        return true
      }
    }
    false
  }
  
  def usedInColumn(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int, size: Int): Boolean = {
    for(i <- 0 to size-1){
      if(board(i)(cIndex) == Some(number)){
        return true
      }
    }
    false
  }
  
  def usedInBox(board: Seq[Seq[Option[Int]]], boxStartRow: Int, boxStartColumn: Int, number: Int, size: Int): Boolean = {
    val n = Math.sqrt(size).toInt
    for(i <- boxStartRow to boxStartRow+n-1){
      for(j <- boxStartColumn to boxStartColumn+n-1){
        if(board(i)(j) == Some(number)){
          return true
        }
      }
    }
    false
  }
  
  def isSafe(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int, size: Int): Boolean = {
    val n = Math.sqrt(size).toInt
    !usedInRow(board, rIndex, 0, number, size) && !usedInColumn(board, 0, cIndex, number, size) && !usedInBox(board, rIndex - (rIndex%n), cIndex - (cIndex%n), number, size)
  }
}

