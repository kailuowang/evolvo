package evolvo
import cats.data.Chain
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import cats.implicits._

import scala.util.Random

class SocietySuite extends ScalaCheckSuite {

  implicit val arbIndividual = Arbitrary(
    Gen.chooseNum(10, 100).map(Individual(_))
  )

  def show(circle: Circle): String =
    s"""
       |      Members: ${circle.members.size}
       |      Top Power: ${circle.topPower}
       |      Bottom Power: ${circle.bottomPower}
       |""".stripMargin

  def show(s: Society, generation: Int): String = {

    s"""
       |+++++++++++++++++++++++
       |Generation $generation
       |
       |  Top circle: ${s.topCircle.map(show).getOrElse("")}
       |  Bottom circle: ${s.bottomCircle.map(show).getOrElse("")}
       |  Num of circles ${s.circles.size}
       |  Population: ${s.population}
       |""".stripMargin
  }

  test("new generation") {
    def gaussian(mean: Double, stdDev: Double): Double =
      Random.nextGaussian() * stdDev + mean

    property("organized into circles according to rule") {
      forAll { (candidates: List[Individual]) =>
        val range = 10
        val maxSize = 13
        val society = Society(Chain.nil, null, range, maxSize)
        val r = candidates.foldLeft(society)((s, i) => s.addMember(i))
        r.circles.toIterable.foreach { circle =>
          val powers = circle.members.map(_.power)
          (powers.maximumOption, powers.minimumOption)
            .mapN((max, min) => assert((clue(max) - clue(min)) < clue(range)))

          assert(clue(circle.members.size) <= maxSize)
        }

        assertEquals(
          candidates.size.toLong,
          r.circles.map(_.members.size).toList.sum
        )

      }

    }

    val initPopulation = List.fill(5000)(Individual(gaussian(100, 15).toInt))
    val reproduction =
      Reproduction(powerChangeMean = -1, powerChangeStdDev = 5, 2.8, 0.5)

    val society = initPopulation.foldLeft(
      Society(Chain.nil, reproduction, circleRange = 15, 100)
    )((s, i) => s.addMember(i))

    val nOfGeneration = 10

    val r = (1 to nOfGeneration - 1).foldLeft(society) { (s, i) =>
      val newGen = s.newGeneration
      println(show(s, i))
      newGen
    }

    println(show(r, nOfGeneration))

    assert(clue(r.population) >= initPopulation.size / 2)

  }
}
