package evolvo

import scala.util.Random
import cats.implicits._

import Math.{max, min}

import cats.Show

import scala.annotation.tailrec
case class Individual(power: Power) extends AnyVal {
  def mate(partner: Individual): Individual = {
    Individual((power + partner.power) / 2)
  }
}

case class Reproduction(powerChangeMeanTowardsPopulationMean: Double,
                        powerChangeStdDev: Double,
                        totalFertilityRate: Double,
                        powerThresholdForReproduction: Power,
                        random: Random = new Random()) {

  def gaussian(mean: Double, stdDev: Double): Double =
    random.nextGaussian() * stdDev + mean

  def mate(father: Individual, mother: Individual): List[Individual] = {
    val numOfChild: Int = {
      val ceil = Math.ceil(totalFertilityRate)
      val floor = Math.floor(totalFertilityRate)
      val useUpperBound = random
        .nextDouble() < (totalFertilityRate - floor.toDouble)

      (if (useUpperBound) ceil
       else floor).toInt
    }

    List.fill(numOfChild) {
      val mean = (father.power + mother.power) / 2d
      val changeEffectMean =
        if (mean > 100) //regression towards the mean
          -powerChangeMeanTowardsPopulationMean
        else
          powerChangeMeanTowardsPopulationMean

      val changeEffect =
        gaussian(changeEffectMean, powerChangeStdDev)

      Individual(max(0, (mean + changeEffect).toInt))
    }
  }

}

case class Circle(private val members: List[Individual],
                  topPower: Power,
                  bottomPower: Power)(val size: Int = members.size) {

  def powers: List[Power] = members.map(_.power)

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
      members
        .filter(_.power > reproduction.powerThresholdForReproduction)
        .splitAt(members.size / 2)

    left.zip(right).flatMap {
      case (father, mother) =>
        reproduction.mate(father, mother)
    }
  }

}

object Circle {
  implicit val showForCircle: Show[Circle] = (circle: Circle) => s"""
       |      Members: ${circle.size}
       |      Top Power: ${circle.topPower}
       |      Bottom Power: ${circle.bottomPower}
       |""".stripMargin
}

case class Society(circles: List[Circle],
                   reproduction: Reproduction,
                   circleRange: Power,
                   maxCircleSize: Int) {

  lazy val population = circles.map(_.size).sum

  lazy val topCircle: Option[Circle] =
    cats.data.NonEmptyList.fromList(circles).map(_.maximumBy(_.topPower))

  lazy val topCircleAveragePower: Option[Int] =
    mergeTopCircles.headOption.map { c =>
      c.powers.sum / c.size
    }

  lazy val populationAveragePower = allPowers.sum / population
  lazy val populationMedianPower =
    allPowers.sorted.get(population.toLong / 2.toLong)

  lazy val topToMedian =
    (topCircleAveragePower, populationMedianPower).mapN(_.toDouble / _)

  def topAndRest: (Option[Circle], List[Circle]) =
    topCircle.fold((none[Circle], circles)) { tc =>
      (topCircle, circles.partition(_ == tc)._2)
    }

  lazy val circleRank =
    circles.toList.sortBy(c => -c.topPower)

  lazy val bottomCircle =
    cats.data.NonEmptyList.fromList(circles).map(_.minimumBy(_.topPower))

  lazy val allPowers = circles.flatMap(_.powers)

  def addMembers(members: List[Individual]): Society =
    members.foldLeft(this)(_.addMember(_))

  lazy val mergeTopCircles: List[Circle] = {
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

  def evolve: Society =
    copy(circles = Nil)
      .addMembers(mergeTopCircles.flatMap(_.newGeneration(reproduction)))

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

object Society {

  def showO[T: Show](o: Option[T]) = o.fold("")(_.show)

  def summary(s: Society) =
    s"""
                               |  Top circles: ${s.circleRank
         .take(5)
         .map(_.show)
         .mkString("\n")}
                               |  Bottom circle: ${s.bottomCircle
         .map(_.show)
         .getOrElse("")}
                               |  Num of circles ${s.circles.size}
                               |  Population: ${s.population}
                               |""".stripMargin

  def report(s: Society) =
    show"""
                              |  Top people average power: ${showO(
            s.topCircleAveragePower
          )}
                              |  Top people size: ${showO(
            s.mergeTopCircles.headOption
              .map(_.size)
          )}
                              |  Population: ${s.population}
                              |  Population average power: ${s.populationAveragePower}
                              |  Population median power: ${showO(
            s.populationMedianPower
          )}
                              |  Top to Median: ${showO(s.topToMedian)}
                              |""".stripMargin

  implicit val showSociety: Show[Society] = (s: Society) => report(s)

  /**
    * IQ statistics based on http://calteches.library.caltech.edu/2797/1/jensen.pdf
    * @param population
    * @param parallelization
    * @return
    */
  def typical(population: Int, parallelization: Int = 4): Society = {
    val random = new Random
    def gaussian(mean: Double, stdDev: Double): Double =
      random.nextGaussian() * stdDev + mean
    val initPopulation =
      List.fill(population)(Individual(gaussian(100, 15).toInt))

    val reproduction =
      Reproduction(
        powerChangeMeanTowardsPopulationMean = 2,
        powerChangeStdDev = 12.5,
        totalFertilityRate = 2.1,
        powerThresholdForReproduction = 50
      )

    Society(Nil, reproduction, circleRange = 20, maxCircleSize = 500)
      .parAddMembers(initPopulation, parallelization)
  }
}
