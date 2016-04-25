import org.specs2.mutable._

class BackTrackerSpec extends Specification {

  "BackTracker" should {
    "Find Unsigned Number correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      val firstLocation = BackTracker.findUnassignedLocation(puzzle.board, 0, 0, 9)
      firstLocation must_== (0,0)
      val secondLocation = BackTracker.findUnassignedLocation(puzzle.board,0,1, 9)
      secondLocation must_== (0,2)
      val nextRow = BackTracker.findUnassignedLocation(puzzle.board, 1,8, 9)
      nextRow must_== (2,1)
    }
    
    "Used in row should check correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      BackTracker.usedInRow(puzzle.board, 0, 0, 9, 9) must_== true
      BackTracker.usedInRow(puzzle.board, 0, 0, 2, 9) must_== false
      BackTracker.usedInRow(puzzle.board, 2, 0, 5, 9) must_== true
      BackTracker.usedInRow(puzzle.board, 2, 0, 3, 9) must_== false
    }
    
    "Used in column should check correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      BackTracker.usedInColumn(puzzle.board, 0, 0, 9, 9) must_== true
      BackTracker.usedInColumn(puzzle.board, 0, 0, 3, 9) must_== false
      BackTracker.usedInColumn(puzzle.board, 2, 7, 6, 9) must_== true
      BackTracker.usedInColumn(puzzle.board, 2, 8, 9, 9) must_== false
    }
    
    "Used in box should check correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      BackTracker.usedInBox(puzzle.board, 0, 0, 8, 9) must_== true
      BackTracker.usedInBox(puzzle.board, 0, 0, 9, 9) must_== false
      BackTracker.usedInBox(puzzle.board, 0, 6, 9, 9) must_== true
      BackTracker.usedInBox(puzzle.board, 0, 6, 6, 9) must_== false
      BackTracker.usedInBox(puzzle.board, 6, 6, 6, 9) must_== true
      BackTracker.usedInBox(puzzle.board, 6, 6, 9, 9) must_== false
    }
    
    "Is safe should check correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      BackTracker.isSafe(puzzle.board, 0, 0, 9, 9) must_== false
      BackTracker.isSafe(puzzle.board, 0, 0, 3, 9) must_== true
      BackTracker.isSafe(puzzle.board, 2, 7, 6, 9) must_== false
      BackTracker.isSafe(puzzle.board, 2, 7, 1, 9) must_== true
      BackTracker.isSafe(puzzle.board, 7, 5, 3, 9) must_== false
      BackTracker.isSafe(puzzle.board, 7, 5, 2, 9) must_== true
      BackTracker.isSafe(puzzle.board, 8, 8, 4, 9) must_== false
      BackTracker.isSafe(puzzle.board, 8, 8, 8, 9) must_== true
    }
    
    "Add number to board correctly" in {
      val test1 = Seq(Seq(Some(1),Some(2)),Seq(None,Some(4)))
      BackTracker.addNumberToBoard(test1, 1, 0, 3) must_== Seq(Seq(Some(1),Some(2)),Seq(Some(3),Some(4)))
      val test2 = Seq(Seq(Some(1),Some(2)),Seq(Some(3),None))
      BackTracker.addNumberToBoard(test2, 1, 1, 4) must_== Seq(Seq(Some(1),Some(2)),Seq(Some(3),Some(4)))
      val test3 = Seq(Seq(Some(1),Some(2),None),Seq(Some(4),None,Some(6)),Seq(None,Some(8),Some(9)))
      BackTracker.addNumberToBoard(
          BackTracker.addNumberToBoard(
              BackTracker.addNumberToBoard(test3, 0,2,3),1,1,5),2,0,7) must_== Seq(
                  Seq(Some(1),Some(2),Some(3)),Seq(Some(4),Some(5),Some(6)),Seq(Some(7),Some(8),Some(9)))
      val test4 = Seq(Seq(None,Some(2),None),Seq(Some(4),None,Some(6)),Seq(None,Some(8),Some(9)))
      BackTracker.addNumberToBoard(
          BackTracker.addNumberToBoard(
            BackTracker.addNumberToBoard(
              BackTracker.addNumberToBoard(test3, 0,0,1),0,2,3),1,1,5),2,0,7) must_== Seq(
                  Seq(Some(1),Some(2),Some(3)),Seq(Some(4),Some(5),Some(6)),Seq(Some(7),Some(8),Some(9)))
    }
    
    "validate baseline correctly" in {
      val puzzle = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baseLineTest)
      val puzzleSolution = BackTracker.solve(puzzle)
      puzzleSolution.size must_== 9
      puzzle.validateSolution(puzzleSolution) must_== true
      val puzzleSolved = SudokuParser.puzzleFromFile(TestSudokuPuzzles.baselineSolution)
      puzzleSolution must_== puzzleSolved.board
    }
  }
}
