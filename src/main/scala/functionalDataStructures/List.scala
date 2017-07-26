package functionalDataStructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
        case Nil => 0 // The sum of the empty list is 0.
        case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
        as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    def append[A](l: List[A], la: List[A]): List[A] = l match {
        case Nil => la
        case Cons(x, xs) => Cons(x, append(xs, la))
    }

    // ex 3.2
    def tail[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_, xs) => xs
    }

    // ex 3.3
    def setHead[A](l: List[A], v: A) = l match {
        case Nil => Nil
        case Cons(_, xs) => Cons(v, xs)
    }

    // ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
        case (Nil, _) => Nil
        case (_, 0) => l
        case (Cons(_, xs), _) => drop(xs, n - 1)
    }

    // ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    // ex 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    // ex 3.10 - tail recursive foldLeft
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
        def foldIter[A, B](l:List[A], acc: B)(f: (B, A) => B): B = l match {
            case Nil => acc
            case Cons(x, xs) => foldIter(xs, f(acc, x))(f)
        }
        foldIter(as, z)(f)
    }

    // ex 3.11
    def length[A](l: List[A]): Int = foldRight(l, 0) ((_, len) => len + 1)

    // ex 3.12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) ((xs, x) => Cons(x, xs))

    // ex 3.14
    def append2[A](l: List[A], v: List[A]): List[A] = foldRight(l, v) ((x, xs) => Cons(x, xs))

    // ex 3.15
    def concatenate[A](l: List[List[A]]): List[A] = l match {
        case Nil => Nil
        case Cons(Nil, _) => Nil
        case Cons(Cons(x, Nil), nl) => Cons(x, concatenate(nl))
        case Cons(Cons(x, xs), nl) => Cons(x, concatenate(Cons(xs, nl)))
    }

    // ex 3.16
    def plus_one(l: List[Int]): List[Int] = l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x + 1, plus_one(xs))
    }

    // ex 3.17
    def to_string(l: List[Double]): List[String] = l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x.toString, to_string(xs))
    }

    // ex 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        def go(sup: List[A], sub: List[A], in: Boolean): Boolean = (sup, sub, in) match {
            case (_, Nil, _) => true
            case (Nil, _, _) => false
            case (Cons(x, xs), Cons(a, as), _) => if (x != a && !in) go(xs, Cons(a, as), false) else if (x != a && in) false else go(xs, as, true)
        }
        go(sup, sub, false)
    }
}
