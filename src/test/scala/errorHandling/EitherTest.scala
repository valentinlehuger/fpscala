package errorHandling

import org.scalatest.{FlatSpec, Matchers}


class EitherTest extends FlatSpec with Matchers {

    "mean(IndexedSeq(1, 2, 3))" should "be Right(2.0)" in {
        Either.mean(IndexedSeq(1, 2, 3)) should be (Right(2.0))
    }

    "mean(IndexedSeq())" should "be Left(\"mean of empty list!\")" in {
        Either.mean(IndexedSeq()) should be (Left("mean of empty list!"))
    }

    "Right(3).map(x => x + 1)" should "be Right(4)" in {
        Right(3).map(x => x + 1) should be (Right(4))
    }

    "Left(\"error\").map(x => x)" should "be Left(\"Error\")" in {
        Left("error").map(x => x) should be (Left("error"))
    }

    "Right(3).flatMap(x => Right(x + 1))" should "be Right(4)" in {
        Right(3).flatMap(x => Right(x + 1)) should be (Right(4))
    }

    "Left(\"error\").flatMap(x => Right(x))" should "be Left(\"Error\")" in {
        Left("error").flatMap(x => Right(x)) should be (Left("error"))
    }

    "Right(3).orElse(Right(4))" should "be Right(3)" in {
        Right(3).orElse(Right(4)) should be (Right(3))
    }

    "Left(\"error\").orElse(Right(4))" should "be Right(4)" in {
        Left("error").orElse(Right(4)) should be (Right(4))
    }

    "Right(1).map2(Right(2))((a, b) => a + b)" should "be Right(3)" in {
        Right(1).map2(Right(2))((a, b) => a + b) should be (Right(3))
    }

    "Right(1).map2(Left(2))((a, b) => a + b)" should "be Left(2)" in {
        Right(1).map2(Left(2))((a, b) => a + b) should be (Left(2))
    }

    "Left(1).map2(Right(2))((a, b) => 42)" should "be Left(1)" in {
        Left(1).map2(Right(2))((a, b) => 42) should be (Left(1))
    }

    "sequence(List(Right(1), Right(2)))" should "be Right(List(1, 2))" in {
        Either.sequence(List(Right(1), Right(2))) should be (Right(List(1, 2)))
    }

    "sequence(List(Right(1), Left(4), Right(2)))" should "be Right(List(1, 2))" in {
        Either.sequence(List(Right(1), Left(4), Right(2))) should be (Left(4))
    }
}
