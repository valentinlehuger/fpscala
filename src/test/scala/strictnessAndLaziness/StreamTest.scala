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

    "Stream(1, 2, 3).takeWhile(_ < 3) using foldRight" should "be Stream(1, 2)" in {
        Stream(1, 2, 3, 2).takeWhileFR(_ < 3).toList should be (List(1, 2))
    }

    "Empty.headOption using foldRight" should "be None" in {
        Empty.headOptionFR should be (None)
    }

    "Stream(1, 2).headOption using foldRight" should "be Some(1)" in {
        Stream(1, 2).headOptionFR should be (Some(1))
    }

}
