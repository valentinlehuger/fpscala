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

    "RNG.double(RNG.Simple(42))._1" should "be between 0 and 1" in {
        val res = RNG.double(RNG.Simple(42))._1
        res > 0.0 && res < 1.0
    }

    "RNG.double(RNG.Simple(-42))._1" should "be between 0 and 1" in {
        val res = RNG.double(RNG.Simple(-42))._1
        res > 0.0 && res < 1.0
    }

    "RNG.double(RNG.Simple(4241241234))._1" should "be between 0 and 1" in {
        val res = RNG.double(RNG.Simple(4241241234L))._1
        res > 0.0 && res < 1.0
    }

    "RNG.intDouble(RNG.Simple(-42))" should "be ((-16159454,0.6006770639683479), RNG.Simple(84537781268864))" in {
        RNG.intDouble(RNG.Simple(-42)) should be ((-16159454, 0.6006770639683479), RNG.Simple(84537781268864L))
    }
   
    "RNG.doubleInt(RNG.Simple(-42))" should "be ((0.6006770639683479, -16159454), RNG.Simple(84537781268864))" in {
        RNG.doubleInt(RNG.Simple(-42)) should be ((0.6006770639683479, -16159454), RNG.Simple(84537781268864L))
    }

    "RNG.double3(RNG.Simple(-42))" should "be ((0.9924751683103271,0.6006770639683479,0.3249912906088826), RNG.Simple(45738457961355))" in {
        RNG.double3(RNG.Simple(-42)) should be ((0.9924751683103271,0.6006770639683479,0.3249912906088826), RNG.Simple(45738457961355L))
    }

    "RNG.ints(5)(RNG.Simple(-42))" should "be List(1, 2, 3, 4, 5)" in {
        RNG.ints(5)(RNG.Simple(-42)) should be (List(-16159454, 1289944172, 697913482, -761862531, 1361908147), RNG.Simple(89254012344509L))
    }
}