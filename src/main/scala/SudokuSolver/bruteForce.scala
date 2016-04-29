object BruteForce extends SudokuSolver{
  type Board = IndexedSeq[IndexedSeq[Int]]
  
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]] = {
    val asSeq = Puzzle.boardToSeq(puzzle.board).map(_.map(_.getOrElse(0)))
    val size = asSeq.size
    val s = Math.sqrt(size).toInt
    val solution = (solveBoard(asSeq, size = size, square = s).getOrElse(IndexedSeq.empty[IndexedSeq[Int]])).toSeq
    solution.map(_.toSeq.map(Some(_)))
  }
  
  def solveBoard(board: Board, cell: Int = 0, size: Int, square: Int): Option[Board] = (cell%size, cell/size) match {
    case (rIndex, `size`) => Some(board)
    case (rIndex, c) if board(rIndex)(c) > 0 => solveBoard(board, cell + 1, size, square)
    case (rIndex, c) =>
      def cells(i: Int) = Seq(board(rIndex)(i), board(i)(c), board(square*(rIndex/square) + i/square)(square*(c/square) + i%square))

      def tryNumber(x: Int): Option[Board] = solveBoard(board.updated(rIndex, board(rIndex).updated(c, x)), cell + 1, size, square)
      val numbers = 1 to size
      val openNumbers = numbers.diff(board.indices.flatMap(cells)) 
      openNumbers.collectFirst(Function.unlift(tryNumber))
  }
}
