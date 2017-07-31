package errorHandling


trait Option[+A] {
    def map[B](f:A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

}
