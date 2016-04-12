import org.specs2.mutable._

class SudokuParserSpec extends Specification {

  "Parser Puzzle Test" should {
    "create default of none" in {
      SudokuParser.puzzleFromFile("test") must_== Puzzle("test", None, None, Vector.empty)
    }

    "have a parsed board 9x9" in {
      val puzzle = SudokuParser.puzzleFromFile("puzzles/puzzle1")
      puzzle.size must_== Some("9x9")
      puzzle.name must_== "puzzles/puzzle1"
      puzzle.difficulty must_== Some(Puzzle.EASY)
      puzzle.board.size must_== 9
      for {
        line <- puzzle.board
      } yield {
        line.size must_== 9
      }
    }
    
    "parse all boards correctly" in {
      for {
        testCase <- SudukoPuzzles.testCases.keys
      } yield {
        val filename = SudukoPuzzles.testCases.getOrElse(testCase, "")
        val puzzle = SudokuParser.puzzleFromFile(filename)
        puzzle.name must_== filename
        for {
          test <- testCase.split(",")
        } yield {
          test.trim match {
            case "Easy" => Some(Puzzle.EASY)
            case "Medium" => Some(Puzzle.MEDIUM)
            case "Hard" => Some(Puzzle.HARD)
            case "9x9" =>
              puzzle.board.size must_== 9
              for {
                line <- puzzle.board
              } yield {
                line.size must_== 9
              }
            case _ => 1 must_== 1
          }
        }
      }
      1 must_== 1
    }
  }
}
