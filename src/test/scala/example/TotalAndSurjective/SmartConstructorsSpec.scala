package example.TotalAndSurjective

import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SmartConstructorsSpec extends WordSpec with GeneratorDrivenPropertyChecks with Matchers {
  val numberZeroOrLess: Gen[Int] = Gen.chooseNum(Int.MinValue, 0)
  val numberMoreThanZero: Gen[Int] = Gen.chooseNum(1, Int.MaxValue)

  "UppercaseString" must {
    "convert string to uppercase" in {
      forAll { s: String =>
        UppercaseString(s).underlying should be(s.toUpperCase)
      }
    }
  }

  "NonEmptyList" must {
    "return none for an empty list" in {
      NonEmptyList(List()) should be(None)
    }

    "return some for a non empty list" in {
      forAll { l: List[String] =>
        whenever(l.nonEmpty) {
          NonEmptyList(l).map(_.underlying) should be(Some(l))
        }
      }
    }
  }

  "NaturalNumber" must {
    "return none for a number 0 or less" in {
      forAll(numberZeroOrLess) { i =>
        NaturalNumber(i) should be(None)
      }
    }

    "return some for a number greater than 0" in {
      forAll(numberMoreThanZero) { i =>
        NaturalNumber(i).map(_.underlying) should be(Some(i))
      }
    }
  }
}