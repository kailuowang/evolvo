package evolvo
import cats.data.Chain
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import cats.implicits._

class MatchSuite extends ScalaCheckSuite {

  implicit val arbIndividual = Arbitrary(
    Gen.chooseNum(10, 100).map(Individual(_))
  )

  property("matches within range") {
    forAll { (candidates: List[Individual]) =>
      val range = 10
      val couples = Match(range).marry(candidates)

      couples.foreach {
        case (husband, wife) =>
          assert(
            Math.abs(clue(husband.power) - clue(wife.power)) <= clue(range)
          )
      }
    }
  }

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
}
