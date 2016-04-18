import org.specs2.mutable._

//TODO There are many methods and details in the algorithm implement as many as possible
class DancingLinksSpec extends Specification {

  "DancingLinks" should {
    "Produce all solutions correctly" in {
      val puzzles = TestSudokuPuzzles.testSolutions
      for {
        key <- puzzles.keys
      } yield {
        val puzzleSolutionPath = puzzles(key)
        val puzzlePath = TestSudokuPuzzles.testCases(key)
        
        val solution = SudokuParser.puzzleFromFile(puzzleSolutionPath)
        val puzzle = SudokuParser.puzzleFromFile(puzzlePath)
        
        val dancingLinksSolution = DancingLinks.solve(puzzle)
        
        dancingLinksSolution must_== solution.board
      }
      1 must_== 1
    }
  }
}
