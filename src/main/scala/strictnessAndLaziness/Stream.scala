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

}