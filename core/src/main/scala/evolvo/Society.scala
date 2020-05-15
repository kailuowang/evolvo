package evolvo

import scala.util.Random
import cats.implicits._

import scala.collection.parallel.CollectionConverters._
import Math.{max, min}

import scala.annotation.tailrec
case class Individual(power: Power) extends AnyVal {
  def mate(partner: Individual): Individual = {
    Individual((power + partner.power) / 2)
  }
}

case class Reproduction(powerChangeMean: Double,
                        powerChangeStdDev: Double,
                        numOfKidsMean: Double,
                        numOfKidsStdDev: Double,
                        random: Random = new Random()) {

  def gaussian(mean: Double, stdDev: Double): Double =
    random.nextGaussian() * stdDev + mean

  def mate(father: Individual, mother: Individual): List[Individual] = {
    List.fill(gaussian(numOfKidsMean, numOfKidsStdDev).toInt) {
      val mean = (father.power + mother.power) / 2d
      Individual(
        max(0, (mean + gaussian(powerChangeMean, powerChangeStdDev)).toInt)
      )
    }
  }

}

case class Circle(private val members: List[Individual],
                  topPower: Power,
                  bottomPower: Power)(val size: Int = members.size) {

  def powers: Set[Power] = members.map(_.power).toSet

  private[evolvo] def checkSize: Boolean = members.size == size

  private[evolvo] def addMember(individual: Individual,
                                range: Power,
                                maxCircleSize: Int): Option[Circle] = {
    val accepts =
      size < maxCircleSize &&
        (individual.power - bottomPower) < range &&
        (topPower - individual.power) < range

    if (accepts)
      Some(
        Circle(
          individual :: members,
          Math.max(topPower, individual.power),
          Math.min(bottomPower, individual.power)
        )(size + 1)
      )
    else None
  }

  def merge(that: Circle): Circle =
    copy(
      members ++ that.members,
      topPower = max(topPower, that.topPower),
      bottomPower = min(bottomPower, that.bottomPower)
    )(size + that.size)

  def newGeneration(reproduction: Reproduction): List[Individual] = {
    val (left, right) =
      members.splitAt(members.size / 2)
    left.zip(right).flatMap {
      case (father, mother) =>
        reproduction.mate(father, mother)
    }
  }

}

case class Society(circles: List[Circle],
                   reproduction: Reproduction,
                   circleRange: Power,
                   maxCircleSize: Int) {

  lazy val population = circles.map(_.size).sum

  lazy val topCircle: Option[Circle] =
    cats.data.NonEmptyList.fromList(circles).map(_.maximumBy(_.topPower))

  def topAndRest: (Option[Circle], List[Circle]) =
    topCircle.fold((none[Circle], circles)) { tc =>
      (topCircle, circles.partition(_ == tc)._2)
    }

  lazy val circleRank =
    circles.toList.sortBy(c => -c.topPower)

  lazy val bottomCircle =
    cats.data.NonEmptyList.fromList(circles).map(_.minimumBy(_.topPower))

  def addMembers(members: List[Individual]): Society =
    members.foldLeft(this)(_.addMember(_))

  def evolve: Society = {
    val mergeTopCircles: List[Circle] = {
      @tailrec
      def loop(rest: List[Circle], merged: Circle): List[Circle] =
        rest match {
          case Nil => List(merged)
          case head :: tail if (merged.size < maxCircleSize) =>
            val newMerged = head.merge(merged)
            loop(tail, newMerged)
          case _ => merged :: rest
        }

      circleRank match {
        case head :: tail =>
          loop(tail, head)
        case Nil => Nil
      }
    }

    copy(circles = Nil)
      .addMembers(mergeTopCircles.flatMap(_.newGeneration(reproduction)))
  }

  def show(circle: Circle): String =
    s"""
       |      Members: ${circle.size}
       |      Top Power: ${circle.topPower}
       |      Bottom Power: ${circle.bottomPower}
       |""".stripMargin

  def parEvolve(numOfParallelization: Int): Society = {
    val subSize = circles.size / numOfParallelization
    if (subSize > 0) {
      val subSocieties =
        circles
          .sliding(subSize, subSize)
          .map(cs => copy(circles = cs))
          .toList
          .par
      subSocieties
        .map(_.evolve)
        .reduce { (l, r) =>
          l.copy(circles = l.circles ++ r.circles)
        }
    } else evolve
  }

  def parAddMembers(members: List[Individual], parallelization: Int): Society =
    if (members.isEmpty) this
    else {
      val subSize = members.size / parallelization
      if (subSize > 0)
        members
          .sliding(subSize, subSize)
          .toList
          .par
          .map(cs => copy(circles = Nil).addMembers(cs))
          .foldLeft(this) { (l, r) =>
            l.copy(circles = l.circles ++ r.circles)
          } else addMembers(members)
    }

  private[evolvo] def addMember(individual: Individual): Society = {

    val (newCircles, found) = circles.foldLeft((List.empty[Circle], false)) {
      (pair, circle) =>
        val (newList, found) = pair
        if (!found) {
          circle
            .addMember(individual, circleRange, maxCircleSize)
            .fold((circle :: newList, false))(
              newCircle => (newCircle :: newList, true)
            )
        } else
          (circle :: newList, true)

    }
    val toUpdate =
      if (found) newCircles
      else
        Circle(List(individual), individual.power, individual.power)() :: circles
    copy(circles = toUpdate)

  }
}
