package evolvo

import cats.data.{Chain, State}

import scala.util.Random
import cats.implicits._

case class Individual(power: Power) extends AnyVal {
  def mate(partner: Individual): Individual = {
    Individual((power + partner.power) / 2)
  }
}

case class Reproduction(powerChangeMean: Power,
                        powerChangeStdDev: Power,
                        numOfKidsMean: Double,
                        numOfKidsStdDev: Double) {

  def gaussian(mean: Double, stdDev: Double): Double =
    Random.nextGaussian() * stdDev + mean

  def gaussian(mean: Int, stdDev: Int): Int =
    gaussian(mean.toDouble, stdDev.toDouble).toInt

  def mate(father: Individual, mother: Individual): List[Individual] = {
    List.fill(gaussian(numOfKidsMean, numOfKidsStdDev).toInt) {
      val mean = (father.power + mother.power) / 2
      Individual(
        Math.max(0, mean + gaussian(powerChangeMean, powerChangeStdDev))
      )
    }
  }

}

case class Circle(members: Chain[Individual],
                  topPower: Power,
                  bottomPower: Power) {

  private[evolvo] def addMember(individual: Individual,
                                range: Power,
                                maxCircleSize: Int): Option[Circle] = {
    val accepts =
      members.size < maxCircleSize &&
        (individual.power - bottomPower) < range &&
        (topPower - individual.power) < range

    if (accepts)
      Some(
        Circle(
          members.append(individual),
          Math.max(topPower, individual.power),
          Math.min(bottomPower, individual.power)
        )
      )
    else None
  }

}

case class Society(circles: Chain[Circle],
                   reproduction: Reproduction,
                   circleRange: Power,
                   maxCircleSize: Int) {

  lazy val population = circles.map(_.members.size).fold

  lazy val topCircle =
    cats.data.NonEmptyChain.fromChain(circles).map(_.maximumBy(_.topPower))

  lazy val bottomCircle =
    cats.data.NonEmptyChain.fromChain(circles).map(_.minimumBy(_.topPower))

  def newGeneration: Society = {
    val newOne = copy(circles = Chain.nil)
    circles.foldLeft(newOne) { (soc, circle) =>
      val (left, right) =
        circle.members.toList.splitAt(circle.members.size.toInt / 2)
      left.zip(right).foldLeft(soc) { (s, pair) =>
        val (father, mother) = pair
        reproduction.mate(father, mother).foldLeft(s)(_.addMember(_))
      }
    }
  }

  private[evolvo] def addMember(individual: Individual): Society = {
    val (found, newCircles) =
      circles
        .traverse(
          c =>
            State { (found: Boolean) =>
              if (found)
                (found, c)
              else
                c.addMember(individual, circleRange, maxCircleSize)
                  .fold((false, c))((true, _))
          }
        )
        .run(false)
        .value
    val toUpdate =
      if (found) newCircles
      else
        circles
          .append(Circle(Chain(individual), individual.power, individual.power))
    copy(circles = toUpdate)

  }
}
