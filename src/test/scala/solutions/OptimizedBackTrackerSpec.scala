import org.specs2.mutable._

class OptimizedBackTrackerSpec extends Specification {

  "Optimized Back Tracker" should {
    "Split board into squares correctly" in {
      val testBoard = Seq(
          Seq(1,2,3,4,5,6,7,8,9),
          Seq(10,11,12,13,14,15,16,17,18),
          Seq(19,20,21,22,23,24,25,26,27),
          Seq(28,29,30,31,32,33,34,35,36),
          Seq(37,38,39,40,41,42,43,44,45),
          Seq(46,47,48,49,50,51,52,53,54),
          Seq(55,56,57,58,59,60,61,62,63),
          Seq(64,65,66,67,68,69,70,71,72),
          Seq(73,74,75,76,77,78,79,80,81))
      val testBoardToTrack = testBoard.map( row => row.map(numb => Tracker(List(), Some(numb))))
      val testTrackBoardAsSquares = OptimizedBackTracker.toSquares(testBoardToTrack)
      val testBoardAsSquares = testTrackBoardAsSquares.map(_.map(_.value.getOrElse(0)))
      val testBoardAsTrackNormal = OptimizedBackTracker.fromSquares(testTrackBoardAsSquares)
      val testBoardAsNormal = testBoardAsTrackNormal.map(_.map(_.value.getOrElse(0)))
      testBoardAsSquares must_== Seq(
          Seq(1,2,3,10,11,12,19,20,21),
          Seq(28,29,30,37,38,39,46,47,48),
          Seq(55,56,57,64,65,66,73,74,75),
          Seq(4,5,6,13,14,15,22,23,24),
          Seq(31,32,33,40,41,42,49,50,51),
          Seq(58,59,60,67,68,69,76,77,78),
          Seq(7,8,9,16,17,18,25,26,27),
          Seq(34,35,36,43,44,45,52,53,54),
          Seq(61,62,63,70,71,72,79,80,81))
          
      testBoardAsNormal must_== testBoard
    }
  }
}
