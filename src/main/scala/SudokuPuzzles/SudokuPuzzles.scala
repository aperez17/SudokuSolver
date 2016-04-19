object SudokuPuzzles {
  val testCases = Map(
      "puzzle1, Easy, 9x9" -> "puzzles/puzzle1",
      "puzzle2, Easy, 9x9" -> "puzzles/puzzle2",
      "puzzle3, Easy, 9x9" -> "puzzles/puzzle3",
      "puzzle4, Medium, 9x9" -> "puzzles/puzzle4",
      "puzzle5, Medium, 9x9" -> "puzzles/puzzle5",
      "puzzle6, Hard, 9x9" -> "puzzles/puzzle6",
      "puzzle7, Hard, 9x9" -> "puzzles/puzzle7",
      "puzzle8, Easy, 25x25" -> "puzzles/puzzle8",
      "puzzle9, Hard, 25x25" -> "puzzles/puzzle9")
  lazy val baseLineTest = testCases.getOrElse("puzzle1, Easy, 9x9", "")
}
