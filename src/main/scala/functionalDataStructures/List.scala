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

    // def test_tail(): Unit =
    // {
    //   assert( tail(         Nil ) ==       Nil, "tail of Nil should be Nil")
    //   assert( tail(     List(3) ) ==       Nil, "tail of single-element list should be Nil")
    //   assert( tail( List(1,2,3) ) == List(2,3), "tail of list should be rest")
    // }

    // def test_setHead(): Unit =
    // {
    //   assert( setHead(       Nil, 1 ) ==       Nil, "setHead of empty list should be empty list")
    //   assert( setHead(   List(2), 1 ) ==   List(1), "setHead of single-element list should be two-element list")
    //   assert( setHead( List(3,2), 1 ) == List(1,2), "setHead of two-element list should be three-element list")
    // }

    // def test_drop(): Unit =
    // {
    //   assert( drop( Nil,          0) ==         Nil, "drop of zero elements from empty list is empty list")
    //   assert( drop( Nil,          1) ==         Nil, "drop of one element from empty list is empty list")
    //   assert( drop( Nil,         10) ==         Nil, "drop of many elements from empty list is empty list")
    //   assert( drop( List(3),      0) ==     List(3), "drop of zero elements from single-element list is the list")
    //   assert( drop( List(3),      1) ==         Nil, "drop of one element from single-element list is empty list")
    //   assert( drop( List(3),     10) ==         Nil, "drop of many elements from single-element list is empty list")
    //   assert( drop( List(1,2,3),  0) == List(1,2,3), "drop of zero elements from list is list")
    //   assert( drop( List(1,2,3),  1) ==   List(2,3), "drop of one elements from list is list without 1st element")
    //   assert( drop( List(1,2,3),  2) ==     List(3), "drop of n elements from list is list without 1st n elements")
    //   assert( drop( List(1,2,3), 10) ==         Nil, "drop of too many elements from list is empty list")
    // }

    // def test_dropWhile(): Unit =
    // {
    //   val positive = (x: Int) => x > 0
    //   assert( dropWhile(                  Nil, positive ) ==                  Nil, "dropWhile of empty list should be empty list")
    //   assert( dropWhile(              List(1), positive ) ==                  Nil, "dropWhile of list with single valid element should be empty list")
    //   assert( dropWhile( List( 1,  2,  3,  4), positive ) ==                  Nil, "dropWhile of list with only valid elements should be empty list")
    //   assert( dropWhile( List( 1,  2, -3,  4), positive ) ==          List(-3, 4), "dropWhile of list with two leading valid elements should be list without leading elements")
    //   assert( dropWhile( List( 1, -2, -3,  4), positive ) ==      List(-2, -3, 4), "dropWhile of list with one leading valid element should be list without leading element")
    //   assert( dropWhile( List(-1, -2, -3,  4), positive ) ==  List(-1, -2, -3, 4), "dropWhile of list with no leading valid elements should be same list")
    //   assert( dropWhile( List(-1, -2, -3, -4), positive ) == List(-1, -2, -3, -4), "dropWhile of list with no valid elements should be Nil")
    // }

    // def test_init(): Unit =
    // {
    //   assert( init(         Nil ) ==       Nil, "init of empty list should be empty list")
    //   assert( init(     List(3) ) ==       Nil, "init of single-element-list should be empty list")
    //   assert( init( List(1,2,3) ) == List(1,2), "init of list should not have last element")
    // }

    // def test_length(): Unit =
    // {
    //   assert( length(         Nil ) == 0, "length of empty list is zero")
    //   assert( length(     List(1) ) == 1, "length of single-element list is one")
    //   assert( length( List(1,2,3) ) == 3, "length of n-element list is n")
    // }

    // def test_foldLeft(): Unit =
    // {
    //     assert( foldLeft(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
    //         foldRight( List(1, 2, 3, 4, 5), 0) (_ + _),
    //             "foldLeft should compute the same sum value as foldRight")

    //     assert( foldLeft(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
    //         foldRight( List(1, 2, 3, 4, 5), 1) (_ * _),
    //             "foldLeft should compute the same product value as foldRight")

    //     assert( foldLeft(  List("a", "b", "c"), "") (_ + _) ==
    //         foldRight( List("a", "b", "c"), "") (_ + _),
    //             "foldLeft should compute the same concatenation value as foldRight")
    // }

    // def test_reverse(): Unit =
    // {
    //     assert(reverse(Nil) == Nil, "")
    //     assert(reverse(List(1)) == List(1), "")
    //     assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1), "")
    // }

    // def test_concatenate(): Unit = {
    //     assert(concatenate(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4), "")
    // }

    // def test_plus_one(): Unit = {
    //     assert(plus_one(List(1, 2, 3)) == List(2, 3, 4))
    // }

    // def test_to_string(): Unit = {
    //     assert(to_string(List(1.2, 2.2, 3.2)) == List("1.2", "2.2", "3.2"))
    // }

    // def test_map(): Unit = {
    //     assert( map(List(1, 2, 3)) (_ + 1) == List(2, 3, 4))
    //     assert( map(List(1.2, 2.2, 3.2)) (_.toString) == List("1.2", "2.2", "3.2"))
    // }

    // def test_hasSubsequence(): Unit = {
    //     assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
    //     assert(hasSubsequence(List(1, 2, 3, 4), List(2, 4)) == false)
    //     assert(hasSubsequence(List(1, 2, 3, 4),  Nil) == true)
    //     assert(hasSubsequence(Nil,  Nil) == true)
    //     assert(hasSubsequence(Nil,  List(1, 4)) == false)
    // }

    // def test(): Unit = {
    //     test_tail
    //     test_setHead
    //     test_drop
    //     test_dropWhile
    //     test_init
    //     test_length
    //     test_foldLeft
    //     test_reverse
    //     test_concatenate
    //     test_plus_one
    //     test_to_string
    //     test_map
    //     test_hasSubsequence
    // }
}
