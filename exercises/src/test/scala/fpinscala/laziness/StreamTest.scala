package fpinscala.laziness

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSpecLike, Matchers}

class StreamTest extends FunSpecLike with Matchers with PropertyChecks {

  describe("EXERCISE 1: A Stream converted to a list") {
    it("forces the evaluation of its members") {
      Stream(1, 2, 3, 4, 5).toList should be(List(1, 2, 3, 4, 5))
    }
  }

  describe("EXERCISE 2A: The take method") {
    it("returns the first n members (EXERCISE 2)") {

      val validNumers =
        for (n <- Gen.choose(0, 1000)) yield n

      forAll(validNumers) { (n: Int) =>
        whenever(n >= 0) {
          val list = Stream.from(1).take(n).toList
          list.length should be(n)

          if (n > 0) {
            (0 until n) foreach { x =>
              list(x) should be(x + 1)
            }
          }
        }
      }
    }

    it("returns empty if the Stream is empty") {
      Stream.empty.take(5).toList should be(List())
    }
  }

  describe("EXERCISE 2B: The drop method") {
    it("returns a Stream that skips the first n members") {
      Stream.from(1).drop(2).take(3).toList should be(List(3, 4, 5))
    }

    it("returns empty if the Stream is empty") {
      Stream.empty.drop(5).toList should be(List())
    }
  }

  describe("EXERCISE 3: The takeWhile method in Stream") {
    it("returns all starting elements of a Stream that match the given predicate") {
      Stream.from(1).takeWhile(_ < 5).toList should be(List(1,2,3,4))
    }
  }


}
