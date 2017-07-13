

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
    }

    /*
        Exercice 1 : Stream to List
    */
    def toList : List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    /*
        Exercice 2.1 : Take first n elements
    */
    def take(n: Int) = (this, n) match {
        case (Empty, _) => empty
        case (_, 0) => empty
        case (Cons(h, t), _) => cons(h(), t().take(n - 1))
    }

    /*
        Exercice 2.2 : Skip first n elements
    */
    // def drop(n: integer) = (this, n) match {
    //     case (Empty, _) => Empty
    //     case (_, 0) => this
    //     case (Cons(h, t), _) => t.drop(n - 1)
    // }

}



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