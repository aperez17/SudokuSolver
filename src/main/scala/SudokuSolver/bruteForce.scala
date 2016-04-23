object BruteForce extends SudokuSolver{
  type Board = IndexedSeq[IndexedSeq[Int]]
  
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]] = {
    val asSeq = Puzzle.boardToSeq(puzzle.board).map(_.map(_.getOrElse(0)))
    val n = asSeq.size
    val s = Math.sqrt(n).toInt
    val solution = (solveBoard(asSeq, n = n, s = s).getOrElse(IndexedSeq.empty[IndexedSeq[Int]])).toSeq
    solution.map(_.toSeq.map(Some(_)))
  }
  
  def solveBoard(board: Board, cell: Int = 0, n: Int, s: Int): Option[Board] = (cell%n, cell/n) match {
    case (r, `n`) => Some(board)
    case (r, c) if board(r)(c) > 0 => solveBoard(board, cell + 1, n, s)
    case (r, c) =>
      def cells(i: Int) = Seq(board(r)(i), board(i)(c), board(s*(r/s) + i/s)(s*(c/s) + i%s))

      def guess(x: Int) = solveBoard(board.updated(r, board(r).updated(c, x)), cell + 1, n, s)
        1 to n diff (board.indices flatMap cells) collectFirst Function.unlift(guess)
  }
}
