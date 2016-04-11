import org.specs2.mutable._

class SampleRun extends Specification {

  "Sample Puzzle Test" should {
    "create default of none" in {
      Puzzle.fromFileName("test") must_== Puzzle("test", Puzzle.EASY, 0, Vector.empty)
    }
  }
}