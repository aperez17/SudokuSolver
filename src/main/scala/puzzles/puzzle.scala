object Puzzle {
  val EASY = "Easy"
  val MEDIUM = "Medium"
  val HARD = "hard"
  
  /**
   * TODO IMPLEMENT FROM FILENAME
   * SHOULD OUTPUT A PUZZLE
   **/
  def fromFileName(filename: String): Puzzle = {
    Puzzle(filename, Puzzle.EASY, 0, Vector.empty[Vector[String]])
  }
}

/**
 * @param name: Name of the puzzle
 * @param difficulty: Difficulty of the puzzle
 * @param size the size of the board
 * @param board the 2x2 board we will solve
 */
case class Puzzle(
    name: String,
    difficulty: String,
    size: Int,
    board: Vector[Vector[String]])