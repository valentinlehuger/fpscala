package strictnessAndLaziness

import Stream._
trait Stream[+A] {

    def toList: List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case _ => List()
    }

    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
        case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n-1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
        case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists(p: A => Boolean): Boolean =
        foldRight(false)((item, acc) => p(item) || acc)

    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((item, acc) => acc && p(item))

    def takeWhileFR(p: A => Boolean): Stream[A] =
        foldRight(empty[A])((item, acc) =>
            if (p(item)) cons(item, acc)
            else empty)

    def headOptionFR: Option[A] =
        foldRight(None:Option[A])((item, _) => Some(item))

    def map[B](f: A => B): Stream[B] =
        foldRight(empty[B])((item, acc) => cons(f(item), acc))

    def filter(f: A => Boolean): Stream[A] =
        foldRight(empty[A])((item, acc) =>
            if (f(item)) cons(item, acc)
            else         acc)

    def append[B>:A](newItem: => Stream[B]): Stream[B] =
        foldRight(newItem)((item, acc) => cons(item, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(empty[B])((item, acc) => f(item) append acc)

    def find(p: A => Boolean): Option[A] =
        filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] =
        Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] =
        Stream.cons(n, from(n + 1))

    def fibs(): Stream[Int] = {
        def helper(m: Int, n: Int): Stream[Int] = Stream.cons(m, helper(n, m + n))
        helper(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case Some((nextVal, nextState)) => cons(nextVal, unfold(nextState)(f))
        case None => empty
    }
}