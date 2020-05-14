package evolvo
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import cats.implicits._

class SocietySuite extends ScalaCheckSuite {

  implicit val arbIndividual = Arbitrary(
    Gen.chooseNum(10, 100).map(Individual(_))
  )

  def show(s: Society, generation: Int): String = {

    s"""
       |+++++++++++++++++++++++
       |Generation $generation
       |
       |  Top circles: ${s.circleRank.take(5).map(s.show).mkString("\n")}
       |  Bottom circle: ${s.bottomCircle.map(s.show).getOrElse("")}
       |  Num of circles ${s.circles.size}
       |  Population: ${s.population}
       |""".stripMargin
  }

  property("organized into circles according to rule") {
    forAll { (candidates: List[Individual]) =>
      val range = 10
      val maxSize = 13
      val society =
        Society(
          Nil,
          Reproduction(powerChangeMean = -1, powerChangeStdDev = 5, 2.7, 0.8),
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

  test("new generation") {
    import scala.util.Random

    val parallelization = 12
    def gaussian(mean: Double, stdDev: Double): Double =
      Random.nextGaussian() * stdDev + mean

    val initPopulation = List.fill(3000000)(Individual(gaussian(100, 15).toInt))
    val reproduction =
      Reproduction(powerChangeMean = -1, powerChangeStdDev = 5, 2.7, 0.8)

    val society =
      Society(Nil, reproduction, circleRange = 15, 500)
        .parAddMembers(initPopulation, parallelization)

    println(show(society, 0))

    val nOfGeneration = 20

    val r = (1 to nOfGeneration - 1).foldLeft(society) { (s, i) =>
      val newGen = s.parEvolve(parallelization)
      println(show(newGen, i))
      newGen
    }

    assert(clue(r.population) >= initPopulation.size / 2)

  }
}
