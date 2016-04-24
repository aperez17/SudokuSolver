object Run {
  def main(args: Array[String]) = {
    val BruteForceEngine = SudokuEngine("BruteForce", BruteForce)
    val BackTrackingEngine = SudokuEngine("BackTracking", BackTracker)
    val DancingLinksEngine = SudokuEngine("DancingLinks", DancingLinks)
    println("Run SudokuEngine on all solvers on 9x9")
    val puzzles = SudokuPuzzles.testCases.filter(key => key._1.contains("9x9"))
    val parsedPuzzles = for{
        puzzleLocation <- puzzles.keys
      } yield {
        SudokuParser.puzzleFromFile(puzzles(puzzleLocation))
      }
    BruteForceEngine.solve(parsedPuzzles.toSet)
    BackTrackingEngine.solve(parsedPuzzles.toSet)
    DancingLinksEngine.solve(parsedPuzzles.toSet)
    println("DONE")
    println("Run SudokuEngine on all solvers on 16x16")
    val puzzles16 = SudokuPuzzles.testCases.filter(key => key._1.contains("16x16"))
    val parsedPuzzles16 = for{
        puzzleLocation <- puzzles16.keys
      } yield {
        SudokuParser.puzzleFromFile(puzzles16(puzzleLocation))
      }
    BruteForceEngine.solve(parsedPuzzles16.toSet)
    BackTrackingEngine.solve(parsedPuzzles16.toSet)
    DancingLinksEngine.solve(parsedPuzzles16.toSet)
    println("DONE")
    println("Run SudokuEngine on all solvers on 25x25")
    val puzzles25 = SudokuPuzzles.testCases.filter(key => key._1.contains("25x25"))
    val parsedPuzzles25 = for{
        puzzleLocation <- puzzles25.keys
      } yield {
        SudokuParser.puzzleFromFile(puzzles25(puzzleLocation))
      }
    if(args.nonEmpty){
      BackTrackingEngine.solve(parsedPuzzles25.toSet)
    }
    DancingLinksEngine.solve(parsedPuzzles25.toSet)
    println("DONE")
  }
}
