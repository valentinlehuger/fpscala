package strictnessAndLaziness

import org.scalatest.{FlatSpec, Matchers}


class StreamTest extends FlatSpec with Matchers {

    "Stream(1, 2, 3) toList" should "be List(1, 2, 3)" in {
        Stream(1, 2, 3).toList should be (List(1, 2, 3))
    }

    "Stream(1, 2, 3).take(2)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3).take(2).toList should be (List(1, 2))
    }

    "Stream(1, 2, 3).drop(2)" should "be Stream(3)" in {
        Stream(1, 2, 3).drop(2).toList should be (List(3))
    }

    "Stream(1, 2, 3).takeWhile(_ < 3)" should "be Stream(1, 2)" in {
        Stream(1, 2, 3).takeWhile(_ < 3).toList should be (List(1, 2))
    }
}
