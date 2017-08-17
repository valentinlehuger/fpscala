package purelyFunctionalState

import org.scalatest.{FlatSpec, Matchers}


class RandomTest extends FlatSpec with Matchers {
    "RNG.Simple(42).nextInt" should "be (16159453, RNG.Simple(1059025964525))" in {
        RNG.Simple(42).nextInt should be ((16159453, RNG.Simple(1059025964525L)))
    }

    "RNG.nonNegativeInt(RNG.Simple(42))._1" should "be > 0" in {
        RNG.nonNegativeInt(RNG.Simple(42))._1 > 0
    }

    "RNG.nonNegativeInt(RNG.Simple(-42))._1" should "be > 0" in {
        RNG.nonNegativeInt(RNG.Simple(-42))._1 > 0
    }

    "RNG.nonNegativeInt(RNG.Simple(4241241234))._1" should "be > 0" in {
        RNG.nonNegativeInt(RNG.Simple(4241241234L))._1 > 0
    }
}