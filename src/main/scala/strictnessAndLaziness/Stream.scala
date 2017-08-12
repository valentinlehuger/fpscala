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

    def map2[B](f: A => B): Stream[B] =
        unfold(this){
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        }

    def take2(n: Int): Stream[A] =
        unfold((n, this)) {
            case (x, Cons(h, t)) if x > 0 => Some(h(), (x - 1, t()))
            case _ => None
        }

    def takeWhile2(p: A => Boolean): Stream[A] =
        unfold(this){
            case Cons(h, t) if p(h()) => Some(h(), t())
            case _ => None
        }

    def zipWith[B, C](z: Stream[B])(f: (A, B) => C): Stream[C] =
        unfold((this, z)) {
            case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
            case _ => None
        }

    def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
        case (_, Empty) => true
        case (Cons(h, t), Cons(h2, t2)) if h() == h2() => t().startsWith(t2())
        case _ => false
    }

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
        foldRight((z, Stream(z)))((a,p) => {
        val b2 = f(a,p._1)
        (b2, cons(b2,p._2))
        })._2

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

    def constant2[A](a: A): Stream[A] =
        unfold(a)(x => Some(x, x))

    def from2(n: Int): Stream[Int] =
        unfold(n)(x => Some(x, x + 1))

    def fibs2(): Stream[Int] =
        unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
}