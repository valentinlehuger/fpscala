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


    "tail(Nil)" should "equal Nil" in {
        List.tail(Nil) should equal (Nil)
    }

    "tail(List(3))" should "equal List(3)" in {
        List.tail(List(3)) should equal (Nil)
    }

    "tail(List(1, 2, 3))" should "equal List(2, 3)" in {
        List.tail(List(1, 2, 3)) should equal (List(2, 3))
    }


    "setHead(Nil, 1)" should "equal Nil" in {
        List.setHead(Nil, 1) should equal (Nil)
    }

    "setHead(List(2), 1)" should "equal List(1)" in {
        List.setHead(List(2), 1) should equal (List(1))
    }

    "setHead(List(2, 3), 1)" should "equal List(1, 3)" in {
        List.setHead(List(2, 3), 1) should equal (List(1, 3))
    }


    "drop(Nil, 0)" should "equal Nil" in {
        List.drop(Nil, 0) should equal (Nil)
    }

    "drop(Nil, 1)" should "equal Nil" in {
        List.drop(Nil, 1) should equal (Nil)
    }

    "drop(Nil, 10)" should "equal Nil" in {
        List.drop(Nil, 10) should equal (Nil)
    }

    "drop(List(3), 0)" should "equal Nil" in {
        List.drop(List(3), 0) should equal (List(3))
    }

    "drop(List(3), 1)" should "equal Nil" in {
        List.drop(List(3), 1) should equal (Nil)
    }

    "drop(List(3), 10)" should "equal Nil" in {
        List.drop(List(3), 10) should equal (Nil)
    }

   "drop(List(1, 2, 3), 0)" should "equal Nil" in {
        List.drop(List(1, 2, 3), 0) should equal (List(1, 2, 3))
    }

    "drop(List(1, 2, 3), 1)" should "equal Nil" in {
        List.drop(List(1, 2, 3), 1) should equal (List(2, 3))
    }

    "drop(List(1, 2, 3), 2)" should "equal Nil" in {
        List.drop(List(1, 2, 3), 2) should equal (List(3))
    }

    "drop(List(1, 2, 3), 10)" should "equal Nil" in {
        List.drop(List(1, 2, 3), 10) should equal (Nil)
    }


    "dropWhile(Nil, positive" should "equal Nil" in {
        List.dropWhile(Nil, (x: Int) => x > 0) should equal (Nil)
    }

    "dropWhile(List(1), positive" should "equal Nil" in {
        List.dropWhile(List(1), (x: Int) => x > 0) should equal (Nil)
    }

    "dropWhile(List(-1), positive" should "equal List(-1)" in {
        List.dropWhile(List(-1), (x: Int) => x > 0) should equal (List(-1))
    }

    "dropWhile(List(1, 2, 3, 4), positive" should "equal Nil" in {
        List.dropWhile(List(1, 2, 3, 4), (x: Int) => x > 0) should equal (Nil)
    }

    "dropWhile(List(1, 2, -3, 4), positive" should "equal List(-3, 4)" in {
        List.dropWhile(List(1, 2, -3, 4), (x: Int) => x > 0) should equal (List(-3, 4))
    }

    "dropWhile(List(1, -2, -3, 4), positive" should "equal List(-2, -3, 4)" in {
        List.dropWhile(List(1, -2, -3, 4), (x: Int) => x > 0) should equal (List(-2, -3, 4))
    }


    "init(Nil)" should "be Nil" in {
        List.init(Nil) should be (Nil)
    }

    "init(List(3))" should "be Nil" in {
        List.init(List(3)) should be (Nil)
    }

    "init(List(1, 2, 3))" should "be List(1, 2)" in {
        List.init(List(1, 2, 3)) should be (List(1, 2))
    }


    "length(Nil)" should "be 0" in {
        List.length(Nil) should be (0)
    }

    "length(List(1))" should "be 1" in {
        List.length(List(1)) should be (1)
    }

    "length(List(1, 2, 3))" should "be 3" in {
        List.length(List(1, 2, 3)) should be (3)
    }


    "foldLeft(List(1, 2, 3, 4, 5), 0) (_ + _)" should "equal foldRight( List(1, 2, 3, 4, 5), 0) (_ + _)" in {
        List.foldLeft(List(1, 2, 3, 4, 5), 0) (_ + _) should equal (List.foldRight( List(1, 2, 3, 4, 5), 0) (_ + _))
    }

    "foldLeft(List(1, 2, 3, 4, 5), 1) (_ * _)" should "equal foldRight( List(1, 2, 3, 4, 5), 1) (_ * _)" in {
        List.foldLeft(List(1, 2, 3, 4, 5), 1) (_ * _) should equal (List.foldRight( List(1, 2, 3, 4, 5), 1) (_ * _))
    }

    "foldLeft(  List(\"a\", \"b\", \"c\"), \"\") (_ + _)" should "equal foldRight( List(\"a\", \"b\", \"c\"), \"\") (_ + _)" in {
        List.foldLeft(List("a", "b", "c"), "") (_ + _) should equal (List.foldRight(List("a", "b", "c"), "") (_ + _))
    }


    "reverse(Nil)" should "be Nil" in {
        List.reverse(Nil) should be (Nil)
    }

    "reverse(List(1))" should "be List(1)" in {
        List.reverse(List(1)) should be (List(1))
    }

    "reverse(List(1, 2, 3))" should "be List(3, 2, 1)" in {
        List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    }


    "concatenate(List(1, 2), List(3, 4))" should "equal List(1, 2, 3, 4)" in {
        List.concatenate(List(List(1, 2), List(3, 4))) should equal (List(1, 2, 3, 4))
    }


    "plus_one(List(1, 2, 3))" should "equal List(2, 3, 4)" in {
        List.plus_one(List(1, 2, 3)) should equal (List(2, 3, 4))
    }


    "to_string(List(1.2, 2.2, 3.2))" should " equal List(\"1.2\", \"2.2\", \"3.2\")" in {
        List.to_string(List(1.2, 2.2, 3.2)) should equal (List("1.2", "2.2", "3.2"))
    }


    "map(List(1, 2, 3)) (_ + 1)" should "List(2, 3, 4)" in {
        List.map(List(1, 2, 3)) (_ + 1) should equal (List(2, 3, 4))
    }

    "map(List(1.2, 2.2, 3.2)) (_.toString)" should "List(\"1.2\", \"2.2\", \"3.2\")" in {
        List.map(List(1.2, 2.2, 3.2)) (_.toString) should equal (List("1.2", "2.2", "3.2"))
    }


    "hasSubsequence(List(1, 2, 3, 4), List(2, 3))" should "be true" in {
        List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
    }

    "hasSubsequence(List(1, 2, 3, 4), List(2, 4))" should "be false" in {
        List.hasSubsequence(List(1, 2, 3, 4), List(2, 4)) should be (false)
    }

    "hasSubsequence(List(1, 2, 3, 4), Nil)" should "be true" in {
        List.hasSubsequence(List(1, 2, 3, 4), Nil) should be (true)
    }

    "hasSubsequence(Nil, Nil)" should "be true" in {
        List.hasSubsequence(Nil, Nil) should be (true)
    }

    "hasSubsequence(Nil, List(1, 4))" should "be false" in {
        List.hasSubsequence(Nil, List(1, 4)) should be (false)
    }
}