import java.io._
import scala.io._

object SudukoParser {
  
 /**
  * Takes a file and outputs a suduko puzzle in the format:
  * Difficulty:Easy
  * Size:9x9
  * ... where ... contains the board
  * @param filename: file that contains puzzle information
  */
 def puzzleFromFile(filename: String): Puzzle = {
   val lines = try {
     Source.fromURL(getClass.getResource(filename)).getLines.toList
   } catch {
     case _: Throwable => List.empty[String]
   }
   val z = Puzzle(filename)
   val (puzzleNoBoard, board) = getBasicInformation(z, lines)
   parseBoardFromLines(puzzleNoBoard, board)
 }
 
 /**
  * Grabs the difficulty and size from the file lines and outputs the lines that contain the board as well
  */
 def getBasicInformation(z: Puzzle, lines: List[String]): (Puzzle, List[String]) = {
   lines.foldLeft(z, List.empty[String]){ case ((puzzle, boardLines), line) => 
     line.trim().split(":").toVector match {
       case Vector("Difficulty", difficulty) => (puzzle.copy(difficulty = Some(difficulty.trim)), boardLines)
       case Vector("Size", size) => (puzzle.copy(size = Some(size.trim)), boardLines)
       case _ => 
         if(line.trim().length() > 0){
           (puzzle, boardLines :+ line.trim)
         } else {
           (puzzle, boardLines)
       }
     }
   }
 }
 
 /**
  * Grab the board from the puzzle
  */
 def parseBoardFromLines(puzzle: Puzzle, lines: List[String]): Puzzle = {
   val board = lines.foldLeft(Vector.empty[Vector[Option[Int]]]) { case (board, lines) =>
     board :+ lines.trim.split(",").toVector.map{ digit =>
       if(digit == "x"){
         None
       } else {
         Some(digit.toInt)
       }
     }
   }
   puzzle.copy(board = board)
 }
}