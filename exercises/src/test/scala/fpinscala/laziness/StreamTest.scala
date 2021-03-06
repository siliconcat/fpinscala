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

  describe("EXERCISE 4: The forAll method ") {
    it("returns true if all the elements in a stream match the predicacte") {
      Stream(1,2).forAll(_ < 5) should be(true)
    }
    it("terminates early if an element does not match the predicate") {
      Stream.from(1).forAll(_ < 5) should be(false)
    }
  }

  describe("EXERCISE 6: The headOption method") {
    it("gets the head of the Stream if not empty") {
      Stream.from(1).headOption should be(Some(1))
    }

    it ("returns None if the Stream is empty") {
      Stream.empty.headOption should be(None)
    }
  }

  describe("EXERCISE 7: Implement map, filter, append, and flatMap using foldRight") {
    it("map") {
      Stream.from(1).map(x => x + 1).take(3).toList should be(List(2,3,4))
    }

    it("filter") {
      Stream.from(1).filter(_ % 2 == 0).take(3).toList should be(List(2,4,6))
    }

    it("append") {
      Stream(1).append(Stream(2,3)).toList should be(List(1,2,3))
    }

    it("flatMap") {
      Stream.from(1).flatMap(x => Stream(x * 3, x * (-3))).take(4).toList should be(List(3,-3,6,-6))
    }
  }

  describe("EXERCISE 8: The constant function") {
    it("returns an infinite Stream of a given value") {
      Stream.constant(5).take(5).toList should be(List(5,5,5,5,5))
    }
  }

  describe("EXERCISE 9: The from function") {
    it("generates an infinite stream of integers, starting from, then + 1,n + 2, and so on") {
      Stream.from(1).take(5).toList should be(List(1,2,3,4,5))
    }
  }

  describe("EXERCISE 10: The fibs function") {
    it("generates the infinite stream of Fibonacci numbers") {
      Stream.fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("EXERCISE 11: The unfold function") {
    it("takes an initial state, and a function for producing both the next state and the next value in the generated stream") {
      val z = Stream.unfold[Int,Stream[Int]](Stream.empty) {
        case s@Cons(h, _) => Some((h() * 3, s))
        case s => Some(1, s)
      }

      z.take(5).toList should be(List(1,3,6,9,12))
    }
  }

}
