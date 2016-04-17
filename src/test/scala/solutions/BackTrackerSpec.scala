import org.specs2.mutable._

class BackTrackerSpec extends Specification {

  "Puzzles" should {
    "validate baseline correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(SudokuPuzzles.baseLineTest)
      val puzzleSolution = BackTracker.solve(puzzle)
      puzzleSolution.size must_== 1
    }
  }
}
