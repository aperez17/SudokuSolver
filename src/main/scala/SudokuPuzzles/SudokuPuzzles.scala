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
      "puzzle9, Medium, 16x16" -> "puzzles/puzzle9",
      "puzzle10, Easy, 16x16" -> "puzzles/puzzle10",
      "puzzle12, Medium, 9x9" -> "puzzles/puzzle12",
      "puzzle13, Hard, 9x9" -> "puzzles/puzzle13")
  lazy val baseLineTest = testCases.getOrElse("puzzle1, Easy, 9x9", "")
}
