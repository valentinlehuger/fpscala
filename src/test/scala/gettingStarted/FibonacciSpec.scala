package gettingStarted

import org.scalatest.{FlatSpec, Matchers}


class FibonacciSpec extends FlatSpec with Matchers {
  "Fibonacci 0" should "be 0" in {
    Fibonacci.fib(0) should be (0)
  }

  "Fibonacci 1" should "be 1" in {
    Fibonacci.fib(1) should be (1)
  }

  "Fibonacci 2" should "be 1" in {
    Fibonacci.fib(2) should be (1)
  }

  "Fibonacci 3" should "be 2" in {
    Fibonacci.fib(3) should be (2)
  }

  "Fibonacci 4" should "be 3" in {
    Fibonacci.fib(4) should be (3)
  }

  "Fibonacci 5" should "be 5" in {
    Fibonacci.fib(5) should be (5)
  }
}