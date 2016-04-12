import org.specs2.mutable._

class SudukoParserSpec extends Specification {

  "Parser Puzzle Test" should {
    "create default of none" in {
      SudokuParser.puzzleFromFile("test") must_== Puzzle("test", None, None, Vector.empty)
    }
    
    "have a parsed board 9x9" in {
      val puzzle = SudokuParser.puzzleFromFile("puzzle1")
      puzzle.size must_== Some("9x9")
      puzzle.name must_== "puzzle1"
      puzzle.difficulty must_== Some(Puzzle.EASY)
      puzzle.board.size must_== 9
      for {
        line <- puzzle.board
      } yield {
        line.size must_== 9
      }
    }
  }
}