package gettingStarted

object Fibonacci extends App {

    def fib(nth : Int) : Int = {

        def go(n : Int, a : Int, b : Int) : Int =
            if (n <= 0) a
            else go(n - 1, b, a + b)

        go(nth, 0, 1)
    }
}