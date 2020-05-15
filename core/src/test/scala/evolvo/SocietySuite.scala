package evolvo
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import cats.implicits._
import arbitraries._

class SocietySuite extends ScalaCheckSuite {

  property("organized into circles according to rule") {
    forAll { (candidates: List[Individual]) =>
      val range = 10
      val maxSize = 13
      val society =
        Society(
          Nil,
          Reproduction(
            powerChangeMeanTowardsPopulationMean = -1,
            powerChangeStdDev = 5,
            totalFertilityRate = 2.7,
            powerThresholdForReproduction = 1
          ),
          range,
          maxSize
        ).parAddMembers(candidates, 2)

      assertEquals(candidates.size, society.circles.map(_.size).sum)

      def check(society: Society, gen: Int) = {
        if (candidates.size > 10)
          assert(society.population > 0)

        society.circles.foreach { circle =>
          val powers = circle.powers.toList
          (powers.maximumOption, powers.minimumOption)
            .mapN((max, min) => assert((clue(max) - clue(min)) < clue(range)))
          assert(circle.checkSize, s"generation $gen checkSize fail")
          assert(clue(circle.size) <= maxSize, s"generation $gen")
        }
      }

      check(society, 0)
      val finalResult = (0 to 3).foldLeft(society) { (s, i) =>
        val r = s.parEvolve(2)
        check(r, i)
        r
      }
      assert(finalResult != null)

    }
  }

}

class Study extends munit.FunSuite {

  test("new generation".ignore) {

    val parallelization = 4

    val initPopulation = 300000
    val society = Society.typical(initPopulation, parallelization)
    println("Initial population")
    println(society.show)

    val nOfGeneration = 5

    val r = (1 to nOfGeneration - 1).foldLeft(society) { (s, i) =>
      val newGen = s.parEvolve(parallelization)
      println(show"""
          |
          |
          |
          |====================Generation $i =====================
          |$newGen
          |""".stripMargin)
      newGen
    }

    assert(clue(r.population) >= initPopulation / 2)

  }
}
