object Puzzle {
  val EASY = "Easy"
  val MEDIUM = "Medium"
  val HARD = "hard"
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
}