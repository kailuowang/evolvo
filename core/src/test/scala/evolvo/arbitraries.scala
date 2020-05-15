package evolvo

import org.scalacheck.{Arbitrary, Gen}

object arbitraries {
  implicit val arbIndividual = Arbitrary(
    Gen.chooseNum(10, 100).map(Individual(_))
  )
}
