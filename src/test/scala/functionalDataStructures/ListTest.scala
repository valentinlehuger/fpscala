package functionalDataStructures

import org.scalatest.{FlatSpec, Matchers}


class ListTest extends FlatSpec with Matchers {

    "sum(Nil)" should "equal 0" in {
        List.sum(Nil) shouldEqual (0)
    }

    "sum(List(5))" should "equal 5" in {
        List.sum(List(5)) shouldEqual (5)
    }

    "sum(List(1, 2, 3, 4))" should "equal 10" in {
        List.sum(List(1, 2, 3, 4)) should equal (10)
    }

    "sum2(Nil)" should "equal 0" in {
        List.sum2(Nil) shouldEqual (0)
    }

    "sum2(List(5))" should "equal 5" in {
        List.sum2(List(5)) shouldEqual (5)
    }

    "sum2(List(1, 2, 3, 4))" should "equal 10" in {
        List.sum2(List(1, 2, 3, 4)) should equal (10)
    }

    "product(Nil)" should "equal 1.0" in {
        List.product(Nil) should equal (1.0)
    }

    "product(List(7.0))" should "equal 7" in {
        List.product(List(7)) should equal (7)
    }

    "product(List(1.0, 2.0, 3.0, 4.0))" should "equal 24.0" in {
        List.product(List(1.0, 2.0, 3.0, 4.0)) should equal (24.0)
    }

    "product(List(1.0, 2.0, 0.0, 4.0))" should "equal 0.0" in {
        List.product(List(1.0, 2.0, 0.0, 4.0)) should equal (0.0)
    }

    "produc2(Nil)" should "equal 1.0" in {
        List.product2(Nil) should equal (1.0)
    }

    "product2(List(7.0))" should "equal 7" in {
        List.product2(List(7)) should equal (7)
    }

    "product2(List(1.0, 2.0, 3.0, 4.0))" should "equal 24.0" in {
        List.product2(List(1.0, 2.0, 3.0, 4.0)) should equal (24.0)
    }

    "product2(List(1.0, 2.0, 0.0, 4.0))" should "equal 0.0" in {
        List.product2(List(1.0, 2.0, 0.0, 4.0)) should equal (0.0)
    }

    "append(Nil, Nil)" should "equal Nil" in {
        List.append(Nil, Nil) should equal (Nil)
    }

    "append(Nil, List(3))" should "equal List(3)" in {
        List.append(Nil, List(3)) should equal (List(3))
    }

    "append(List(3), Nil)" should "equal List(3)" in {
        List.append(List(3), Nil) should equal (List(3))
    }

    "append(List(1, 2), List(3))" should "equal List(1, 2, 3)" in {
        List.append(List(1, 2), List(3)) should equal (List(1, 2, 3))
    }

    "append(List(1), List(2, 3))" should "equal List(1, 2, 3)" in {
        List.append(List(1), List(2, 3)) should equal (List(1, 2, 3))
    }

    "append(List(1, 2), List(3, 4))" should "equal List(1, 2, 3, 4)" in {
        List.append(List(1, 2), List(3, 4)) should equal (List(1, 2, 3, 4))
    }

    "append2(Nil, Nil)" should "equal Nil" in {
        List.append2(Nil, Nil) should equal (Nil)
    }

    "append2(Nil, List(3))" should "equal List(3)" in {
        List.append2(Nil, List(3)) should equal (List(3))
    }

    "append2(List(3), Nil)" should "equal List(3)" in {
        List.append2(List(3), Nil) should equal (List(3))
    }

    "append2(List(1, 2), List(3))" should "equal List(1, 2, 3)" in {
        List.append2(List(1, 2), List(3)) should equal (List(1, 2, 3))
    }

    "append2(List(1), List(2, 3))" should "equal List(1, 2, 3)" in {
        List.append2(List(1), List(2, 3)) should equal (List(1, 2, 3))
    }

    "append2(List(1, 2), List(3, 4))" should "equal List(1, 2, 3, 4)" in {
        List.append2(List(1, 2), List(3, 4)) should equal (List(1, 2, 3, 4))
    }

}