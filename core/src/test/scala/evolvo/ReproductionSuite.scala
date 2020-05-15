package evolvo

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ReproductionSuite extends ScalaCheckSuite {

  property("num of children close to total fertility rate") {
    forAllNoShrink(Gen.choose(2d, 4d)) { fertilityRate =>
      val n = 10000
      val families = List.fill(n)(
        Reproduction(-5, 10, totalFertilityRate = fertilityRate)
          .mate(Individual(100), Individual(100))
      )
      val averageNumOfChildren = families.map(_.size).sum / n.toDouble

      assert(Math.abs(clue(averageNumOfChildren) - clue(fertilityRate)) < 0.1)
    }
  }

}
