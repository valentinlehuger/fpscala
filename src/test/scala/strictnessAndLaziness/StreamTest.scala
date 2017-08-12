package strictnessAndLaziness

import org.scalatest.{FlatSpec, Matchers}


class StreamTest extends FlatSpec with Matchers {

    "Stream(1, 2, 3) toList" should "be List(1, 2, 3)" in {
        Stream(1, 2, 3).toList should be (List(1, 2, 3))
    }

    "Empty.headOption" should "be None" in {
        Empty.headOption should be (None)
    }

    "Stream(1, 2).headOption" should "be Some(1)" in {
        Stream(1, 2).headOption should be (Some(1))
    }

    "Stream(1, 2, 3).take(2)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3).take(2).toList should be (List(1, 2))
    }

    "Stream(1, 2, 3).drop(2)" should "be Stream(3)" in {
        Stream(1, 2, 3).drop(2).toList should be (List(3))
    }

    "Stream(1, 2, 3).takeWhile(_ < 3)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3, 2).takeWhile(_ < 3).toList should be (List(1, 2))
    }

    "Stream(1, 2, 3).forAll(_ < 3)" should "be false" in {
        Stream(1, 2, 3).forAll(_ < 3) should be (false)
    }

    "Stream(1, 2, 3).forAll(_ < 4)" should "be true" in {
        Stream(1, 2, 3).forAll(_ < 4) should be (true)
    }

    "Stream(1, 2, 3, 2).takeWhile(_ < 3) using foldRight" should "be Stream(1, 2)" in {
        Stream(1, 2, 3, 2).takeWhileFR(_ < 3).toList should be (List(1, 2))
    }

    "Empty.headOption using foldRight" should "be None" in {
        Empty.headOptionFR should be (None)
    }

    "Stream(1, 2).headOption using foldRight" should "be Some(1)" in {
        Stream(1, 2).headOptionFR should be (Some(1))
    }

    "Stream(1, 2, 3).map(_ + 42)" should "be Stream(43, 44, 45)" in {
        Stream(1, 2, 3).map(_ + 42).toList should be (List(43, 44, 45))
    }

    "Stream(1, 2, 3).filter(_ != 2)" should "be Stream(1, 3)" in {
        Stream(1, 2, 3).filter(_ != 2).toList should be (List(1, 3))
    }

    "Stream(1, 2, 3).append(Stream(4))" should "be Stream(1, 2, 3, 4)" in {
        Stream(1, 2, 3).append(Stream(4)).toList should be (List(1, 2, 3, 4))
    }

    "Stream(1, 2, 3).flatMap(Stream(_ + 42))" should "be Stream(43, 44, 45)" in {
        Stream(1, 2, 3).flatMap(x => Stream(x + 42)).toList should be (List(43, 44, 45))
    }

    "Stream.constant(42).take(5)" should "be Stream(42, 42, 42, 42, 42)" in {
        Stream.constant(42).take(5).toList should be (List(42, 42, 42, 42, 42))
    }

    "Stream.from(42).take(5)" should "be Stream(42, 43, 44, 45, 46)" in {
        Stream.from(42).take(5).toList should be (List(42, 43, 44, 45, 46))
    }

    "Stream.fibs().take(7)" should "be Stream(0, 1, 1, 2, 3, 5, 8)" in {
        Stream.fibs().take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }

    "Stream.unfold(1)(x => Some(x + 1, x + 2)).take(5)" should "be Stream(2, 4, 6, 8, 10)" in {
        Stream.unfold(1)(x => Some(x + 1, x + 2)).take(5).toList should be (List(2, 4, 6, 8, 10))
    }

    "Stream.constant2(42).take(5)" should "be Stream(42, 42, 42, 42, 42)" in {
        Stream.constant2(42).take(5).toList should be (List(42, 42, 42, 42, 42))
    }

    "Stream.from2(42).take(5)" should "be Stream(42, 43, 44, 45, 46)" in {
        Stream.from2(42).take(5).toList should be (List(42, 43, 44, 45, 46))
    }

    "Stream.fibs2().take(7)" should "be Stream(0, 1, 1, 2, 3, 5, 8)" in {
        Stream.fibs2().take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }

    "Stream(1, 2, 3).map2(_ + 42)" should "be Stream(43, 44, 45)" in {
        Stream(1, 2, 3).map2(_ + 42).toList should be (List(43, 44, 45))
    }

    "Stream(1, 2, 3).take2(2)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3).take2(2).toList should be (List(1, 2))
    }

    "Stream(1, 2, 3, 2).takeWhile2(_ < 3)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3, 2).takeWhile2(_ < 3).toList should be (List(1, 2))
    }

    "Stream(1, 2, 3).zipWith(Stream(4, 5, 6, 7))((x, y) => x * y)" should "be Stream(4, 10, 18)" in {
        Stream(1, 2, 3).zipWith(Stream(4, 5, 6, 7))((x, y) => x * y).toList should be (List(4, 10, 18))
    }

    "Stream(1, 2, 3).startsWith(Stream(1, 2))" should "be true" in {
        Stream(1, 2, 3).startsWith(Stream(1, 2)) should be (true)
    }

    "Stream(1,2,3).scanRight(0)(_ + _)" should "be Stream(6, 5, 3, 0)" in {
        Stream(1,2,3).scanRight(0)(_ + _).toList should be (List(6, 5, 3, 0))
    }
}
