object BackTracker extends SudokuSolver {
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]] = {
    val (newBoard, value) = solveBackTrack(puzzle.board, 0, 0)
    val newPuzzle = puzzle.copy(board = newBoard)
    newPuzzle.printBoard()
    if(value == false){
      puzzle.board
    } else {
      newBoard
    }
  }
  
  def solveBackTrack(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int): (Seq[Seq[Option[Int]]], Boolean) = {
    val (newRow, newColumn) = findUnassignedLocation(board, 0, 0)
    if((newRow, newColumn) == (-1,-1)){
      (board, true)
    } else {
      val allNumbs = (for {
        i <- 1 to board.size
      } yield {
        i
      })
      
      for (i <- allNumbs){
        if(isSafe(board, newRow, newColumn, i)){
          val newBoard = addNumberToBoard(board, newRow, newColumn, i)
          if(solveBackTrack(newBoard, newRow, newColumn)._2){
            (newBoard, true)
          }
        }
      }
      (board, false)
    }
  }
  
  def addNumberToBoard(board :Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int): Seq[Seq[Option[Int]]] = {
    board.foldLeft(Seq.empty[Seq[Option[Int]]]) { case (newBoard, row) => 
      val r = board.indexOf(row)
      val newRow = row.foldLeft(Seq.empty[Option[Int]]) { case (nr, numbOpt) => 
        val c = row.indexOf(numbOpt)
        
        if(r == rIndex && c == cIndex){
          nr :+ Some(number)
        } else {
          nr :+ numbOpt
        }
      }
      newBoard :+ newRow
    }
  }
  
  def findUnassignedLocation(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int): (Int, Int) = {
    val size = board.size
    board(rIndex)(cIndex) match {
      case Some(number) if(cIndex < size-1)  => findUnassignedLocation(board, rIndex, cIndex+1)
      case Some(number) if(rIndex < size-1)  => findUnassignedLocation(board, rIndex+1, 0)
      case Some(number) if(cIndex == size-1 && rIndex == size-1)=> (-1,-1)//NOT FOUND
      case None => (rIndex, cIndex)
    }
  }
  
  def usedInRow(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int): Boolean = {
    val size = board.size
    board(rIndex)(cIndex) match {
      case Some(numb) if(cIndex == size-1) => number == numb
      case Some(numb) => if(numb == number) true else usedInRow(board,rIndex, cIndex + 1, number)
      case None if(cIndex == size-1) => false
      case None => usedInRow(board, rIndex, cIndex + 1, number)
    }
  }
  
  def usedInColumn(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int): Boolean = {
    val size = board.size
    board(rIndex)(cIndex) match {
      case Some(numb) if(rIndex == size-1) => number == numb
      case Some(numb) => if(numb == number) true else usedInColumn(board, rIndex + 1, cIndex, number)
      case None if(rIndex == size-1) => false
      case None => usedInColumn(board, rIndex + 1, cIndex, number)
    }
  }
  
  def usedInBox(board: Seq[Seq[Option[Int]]], boxStartRow: Int, boxStartColumn: Int, number: Int): Boolean = {
    val n = Math.sqrt(board.size).toInt
    board(boxStartRow)(boxStartColumn) match {
      case Some(numb) if(boxStartRow%n == n-1) => if(number == numb) true else usedInBox(board, boxStartRow+1-n, boxStartColumn + 1, number)
      case Some(numb) if(boxStartColumn%n == n-1) => number == numb
      case Some(numb) => if(numb == number) true else usedInBox(board, boxStartRow + 1, boxStartColumn, number)
      case None if(boxStartRow%n == n-1) => usedInBox(board, boxStartRow+1-n, boxStartColumn+1, number)
      case None if(boxStartColumn%n == n-1) => false
      case None => usedInBox(board, boxStartRow + 1, boxStartColumn, number)
    }
  }
  
  def isSafe(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int, number: Int): Boolean = {
    val n = Math.sqrt(board.size).toInt
    !usedInRow(board, rIndex, 0, number) && !usedInColumn(board, 0, cIndex, number) && !usedInBox(board, rIndex - (rIndex%n), cIndex - (cIndex%n), number)
  }
}

