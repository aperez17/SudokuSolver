// $COVERAGE-OFF$Disabling highlighting by default until a workaround for https://issues.scala-lang.org/browse/SI-8596 is found
case class SudokuEngine(
    algorithm: String,
    implementation: SudokuSolver){
  
  /**
   * Solve all the puzzles and output the result to true if all
   * puzzles were successfully passed
   * @param puzzles: filenames for the puzzles to solve
   */
  def solve(puzzles: Set[Puzzle]) = {
    println()
    println("======================")
    println("RUNNING: " + algorithm)
    val results = puzzles.map{  puzzle => 
        if(puzzle.board.isEmpty){
          (println("NO BOARD TO SOLVE, POSSIBLE PARSING FAILURE"), 0L)
        } else {
          val s0 = System.nanoTime()
          val puzzleSolution = implementation.solve(puzzle)
          val s1 = System.nanoTime()
          println()
          if(puzzle.validateSolution(puzzleSolution)){
            println(s"Solved: ${puzzle.name} ${puzzle.difficulty.getOrElse("")} ${puzzle.size.getOrElse("")}")
            (println(s"Elapsed time: " + (s1 - s0)/1000000000.0 + "s"), s1-s0)
          } else {
            (println(s"Failed to solve ${puzzle.name}"), 0L)
          }
        }
      }
    val sum = results.foldLeft(0L){case (total, (unit, time)) => total + time}
    println("Total Elapsed time: " + sum/1000000000.0 + "s")
  }
}
// $COVERAGE-ON