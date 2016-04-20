import Math._

object ScalaJSExample{
  def main(): Unit = {
    val n = 9
    val s = Math.sqrt(n).toInt
    type Board = IndexedSeq[IndexedSeq[Int]]

    def solveBackTrack(board: Seq[Seq[Option[Int]]], rIndex: Int, cIndex: Int): (Seq[Seq[Option[Int]]], Boolean) = {

     
    def solve(board: Board, cell: Int = 0): Option[Board] = (cell%n, cell/n) match {
      case (r, `n`) => Some(board)
      case (r, c) if board(r)(c) > 0 => solve(board, cell + 1)
      case (r, c) =>
        def cells(i: Int) = Seq(board(r)(i), board(i)(c), board(s*(r/s) + i/s)(s*(c/s) + i%s))
        
        def guess(x: Int) = solve(board.updated(r, board(r).updated(c, x)), cell + 1)  
     
        1 to n diff (board.indices flatMap cells) collectFirst Function.unlift(guess)
    }
 
    //////////////////////////////////////////////////////////////////
    import scala.collection.{IndexedSeq => $}
    val board = $(                        //0s denote empty cells
      $(1, 0, 0, 0, 0, 7, 0, 9, 0),
      $(0, 3, 0, 0, 2, 0, 0, 0, 8),
      $(0, 0, 9, 6, 0, 0, 5, 0, 0),
      $(0, 0, 5, 3, 0, 0, 9, 0, 0),
      $(0, 1, 0, 0, 8, 0, 0, 0, 2),
      $(6, 0, 0, 0, 0, 4, 0, 0, 0),
      $(3, 0, 0, 0, 0, 0, 0, 1, 0),
      $(0, 4, 0, 0, 0, 0, 0, 0, 7),
      $(0, 0, 7, 0, 0, 0, 3, 0, 0)
    ) 
    val start = now()
    (solve(board).get map {_ mkString " "}).foreach(println(_)) 
    println("_" * 9 * 2)
    println(s"Solved")
  }
}
