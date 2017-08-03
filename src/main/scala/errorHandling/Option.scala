package errorHandling


trait Option[+A] {
    def map[B](f:A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def flatMap[B](f:A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = this match {
        case None => default
        case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if f(a) => this
        case _ => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
        val optAge: Option[Int] = Try(age toInt)
        val optTickets: Option[Int] = Try(numberOfSpeedingTickets toInt)
        map2(optAge, optTickets)(insuranceRateQuote)
    }

    def insuranceRateQuote(age: Int, tickets: Int) = (age + tickets) / 2.0

    def Try[A](a: => A): Option[A] =
        try Some(a)
        catch { case e: Exception => None }

    def sequence[A](a: List[Option[A]]):Option[List[A]] = a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    def sequence2[A](a: List[Option[A]]):Option[List[A]] = 
        traverse(a)(x => x)

    // For-comprehension
    def map2_fc[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for {
            aa <- a
            bb <- b
        } yield f(aa, bb)

}
