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

    "None.map(_ => _ + 1)" should "be None" in {
        None.map(a => a) should be (None)
    }

    "Some(3).flatMap(_ => _ + 1)" should "be Some(4)" in {
        Some(3).flatMap(a => Some(a + 1)) should be (Some(4))
    }

    "Some(3).flatMap(_ => None)" should "be None" in {
        Some(3).flatMap(a => None) should be (None)
    }
    
    "Some(3).getOrElse(2)" should "be 3" in {
        Some(3).getOrElse(2) should be (3)
    }

    "None.getOrElse(2)" should "be 2" in {
        None.getOrElse(2) should be (2)
    }

    "Some(3).orElse(Some(2))" should "be Some(3)" in {
        Some(3).orElse(Some(2)) should be (Some(3))
    }

    "None.orElse(Some(2))" should "be Some(2)" in {
        None.orElse(Some(2)) should be (Some(2))
    }

    "Some(3).filter(x => x < 2)" should "None" in {
        Some(3).filter(x => x < 2) should be (None)
    }
    
    "Some(1).filter(x => x < 2)" should "Some(1)" in {
        Some(1).filter(x => x < 2) should be (Some(1))
    }

    "None.filter(x => x < 2)" should "None" in {
        None.filter(x => true) should be (None)
    }

    "variance(Seq(1.5, 2.2, 3.3))" should "be 1.2" in {
        Option.variance(Seq(1.5, 2.2, 3.3)) should be (Some(0.5488888888888888))
    }

    "parseInsuranceRateQuote(\"5\", \"7\")" should "be 6.0" in {
        Option.parseInsuranceRateQuote("5", "7") should be (Some(6.0))
    }

    "sequence(List(Some(1), Some(2))))" should "be Some(List(1, 2))" in {
        Option.sequence(List(Some(1), Some(2))) should be (Some(List(1, 2)))
    }
}