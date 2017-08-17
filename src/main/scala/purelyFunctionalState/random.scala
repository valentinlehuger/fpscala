package purelyFunctionalState


trait RNG {
    def nextInt: (Int, RNG)
}


object RNG {

    case class Simple(seed: Long) extends RNG {

        def nextInt : (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = Simple(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, next) = rng.nextInt
        if (n < 0) (n - Int.MinValue, next) else (n, next)
    }
}