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

    def double(rng: RNG): (Double, RNG) = {
        val (n, next) = nonNegativeInt(rng)
        (n.toDouble / Int.MaxValue, next)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (n, rng2) = rng.nextInt
        val (m, next) = double(rng2)
        ((n, m), next)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((n, m), next) = intDouble(rng)
        ((m, n), next)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (n1, next1) = double(rng)
        val (n2, next2) = double(next1)
        val (n3, next3) = double(next2)
        ((n1, n2, n3), next3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        if (count <= 0) (Nil, rng) else {
            val (n, next) = rng.nextInt
            val (tail, next2) = ints(count - 1)(next)
            (n :: tail, next2)
        }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt


    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def nonNegativeEven: Rand[Int] =
        map(nonNegativeInt)(i => i - i % 2)

    def double_2: Rand[Double] =
        map(nonNegativeInt)(i => i / Int.MaxValue)
}