package errorHandling

import org.scalatest.{FlatSpec, Matchers}


class OptionTest extends FlatSpec with Matchers {

    "mean(Seq(1, 2, 3))" should "equal Some(6)" in {
        Option.mean(Seq(1, 2, 3)) should equal (Some(2))
    }

    "mean(Seq())" should "be None" in {
        Option.mean(Seq()) should be (None)
    }

    "Some(3).map(_ => _ + 1)" should "be Some(4)" in {
        Some(3).map(a => a + 1) should be (Some(4))
    }
}