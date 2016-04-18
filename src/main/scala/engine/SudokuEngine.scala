case class SudokuEngine(
    algorithm: String,
    implementation: SudokuSolver){
  
  /**
   * Solve all the puzzles and output the result to true if all
   * puzzles were successfully passed
   * @param puzzles: filenames for the puzzles to solve
   */
  def solve(puzzles: Set[Puzzle]) = {
    time {
      puzzles.map{  puzzle => 
        if(puzzle.board.isEmpty){
          println("NO BOARD TO SOLVE, POSSIBLE PARSING FAILURE")
          false
        } else {
          val puzzleSolution = implementation.solve(puzzle)
          if(puzzle.validateSolution(puzzleSolution)){
            println("Solved:")
            Puzzle.printBoard(puzzle.name, puzzle.difficulty, puzzle.size, puzzleSolution)
          } else {
            println(s"Failed to solve ${puzzle.name}")
          }
        }
      }
    }
  }
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}