import org.specs2.mutable._

class PuzzlesSpec extends Specification {

  "Puzzles" should {
    "validate baseline correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(SudokuPuzzles.baseLineTest)
      val puzzleSolution = SudokuParser.puzzleFromFile(SudokuPuzzles.baselineSolution)
      puzzle.validateSolution(puzzleSolution.board) must_== true
    }
    
    "validate all solutions correctly" in {
      for {
        testCase <- SudokuPuzzles.testSolutions.keys
      } yield {
        val filename = SudokuPuzzles.testCases.getOrElse(testCase, "")
        val puzzuleSolutionFile = SudokuPuzzles.testSolutions.getOrElse(testCase, "")
        
        val puzzle = SudokuParser.puzzleFromFile(filename)
        val puzzleSolution = SudokuParser.puzzleFromFile(puzzuleSolutionFile)
        puzzle.validateSolution(puzzleSolution.board) must_== true
      }
      1 must_== 1
    }
    
    "invalidate invalid solutions" in {
      for {
        testCase <- SudokuPuzzles.testSolutions.keys
      } yield {
        val filename = SudokuPuzzles.testCases.getOrElse(testCase, "")
        val puzzuleSolutionFile = SudokuPuzzles.testSolutionsBAD.getOrElse(testCase, "")
        
        val puzzle = SudokuParser.puzzleFromFile(filename)
        val puzzleSolution = SudokuParser.puzzleFromFile(puzzuleSolutionFile)
        puzzle.validateSolution(puzzleSolution.board) must_== false
      }
      1 must_== 1
    }
    
    "invalidate valid solutions that alter original board" in {
      val puzzle = SudokuParser.puzzleFromFile(SudokuPuzzles.baseLineTest)
      val solution2 = SudokuPuzzles.testSolutions.getOrElse("puzzle2, Easy, 9x9", "")
      val puzzleSolution = SudokuParser.puzzleFromFile(solution2)
      puzzle.validateSolution(puzzleSolution.board) must_== false
    }
  }
}
