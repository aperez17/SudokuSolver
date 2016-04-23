import org.specs2.mutable._

class BruteForceSpec extends Specification {

  "Brute Force" should {
    "validate baseline correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      val puzzleSolution = BruteForce.solve(puzzle)
      puzzleSolution.size must_== 9
      puzzle.validateSolution(puzzleSolution) must_== true
    }
    
    "solve first test correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      val puzzleSolution = BruteForce.solve(puzzle)
      val solution = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baselineSolution)
      puzzleSolution must_== solution.board
    }
  }
}
