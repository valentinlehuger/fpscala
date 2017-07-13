
object Fibonacci {

    def fib(nth : Int) : Int = {

        def go(n : Int, a : Int, b : Int) : Int =
            if (n <= 0) a
            else go(n - 1, b, a + b)

        go(nth, 0, 1)
    }

    def format() : String = {
        val res = "%d %d %d %d"
        res.format(fib(0), fib(1), fib(2), fib(3))
    }

    def main(args : Array[String]) : Unit =
        println(format())
}
