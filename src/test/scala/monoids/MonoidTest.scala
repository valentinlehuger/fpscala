package monoids

import org.scalatest.{FlatSpec, Matchers}


class MonoidTest extends FlatSpec with Matchers {

  "stringMonoid(abc, cde)" should "equal abcde" in {
    Monoid.stringMonoid.op("abc", "cde") should equal ("abccde")
  }

  "stringMonoid(cde, zero)" should "equal cde" in {
    Monoid.stringMonoid.op("cde", Monoid.stringMonoid.zero) should equal ("cde")
  }

  "listMonoid(List(1, 2, 3), zero)" should "equal List(1, 2, 3)" in {
    Monoid.listMonoid.op(List(1, 2, 3), Monoid.listMonoid.zero) should equal (List(1, 2, 3))
  }

  "listMonoid(List(1, 2, 3), List(4, 5))" should "equal List(1, 2, 3, 4, 5)" in {
    Monoid.listMonoid.op(List(1, 2, 3, 4, 5), Monoid.listMonoid.zero) should equal (List(1, 2, 3, 4, 5))
  }

  "intAddition(3, 2)" should "equal 5" in {
    Monoid.intAddition.op(3, 2) should equal (5)
  }

  "intAddition(3, zero)" should "equal 3" in {
    Monoid.intAddition.op(3, Monoid.intAddition.zero) should equal (3)
  }

  "intMultiplication(3, 2)" should "equal 6" in {
    Monoid.intMultiplication.op(3, 2) should equal (6)
  }

  "intMultiplication(3, zero)" should "equal 3" in {
    Monoid.intMultiplication.op(3, Monoid.intMultiplication.zero) should equal (3)
  }

  "booleanOr(false, true)" should "equal true" in {
    Monoid.booleanOr.op(false, true) should equal (true)
  }

  "booleanOr(false, zero)" should "equal false" in {
    Monoid.booleanOr.op(false, Monoid.booleanOr.zero) should equal (false)
  }
  
  "booleanAnd(false, true)" should "equal false" in {
    Monoid.booleanAnd.op(false, true) should equal (false)
  }

  "booleanAnd(true, zero)" should "equal true" in {
    Monoid.booleanAnd.op(true, Monoid.booleanAnd.zero) should equal (true)
  }
  
  "endoMonoid((x: Int => x + 1), (x: Int => x * 2))" should "equal (x: Int => x * 2 + 1)" in {
    val result: Int => Int = Monoid.endoMonoid.op((x: Int) => x + 1, (x: Int) => x * 2)
    val expected: Int => Int = (x: Int) => x * 2 + 1

    assert(result(5) == 11)
    assert(result(5) == expected(5))
  }  
}
