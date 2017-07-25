package gettingStarted

import org.scalatest.{FlatSpec, Matchers}


class SortedTest extends FlatSpec with Matchers {

    "isSorted(Array(3, 2, 1) sup)" should "be true" in {
        Sorted.isSorted(Array(3, 2, 1), Sorted.sup) should be (true)
    }

    "isSorted(Array(2, 3, 1) sup)" should "be false" in {
        Sorted.isSorted(Array(2, 3, 1), Sorted.sup) should be (false)
    }

    "isSorted(Array('1', '2', '3') inf)" should "be true" in {
        Sorted.isSorted(Array('1', '2', '3'), Sorted.inf) should be (true)
    }

    "isSorted(Array('3', '1', '2') inf)" should "be false" in {
        Sorted.isSorted(Array('3', '1', '2'), Sorted.inf) should be (false)
    }

}
