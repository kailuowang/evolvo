package evolvo

import cats.data.{Chain, State}
import Chain.==:

import scala.util.Random
import cats.implicits._

import scala.annotation.tailrec

case class Individual(power: Power) extends AnyVal {
  def mate(partner: Individual): Individual = {
    Individual((power + partner.power) / 2)
  }
}

case class Reproduction(meanEffect: Power, stdDev: Power) {
  def mate(father: Individual, mother: Individual): Individual = {
    val mean = (father.power + mother.power) / 2
    val effect = Random.nextGaussian() * stdDev + meanEffect
    Individual(Math.max(0, mean + effect.toInt))
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

  def addMember(individual: Individual): Society = {
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

case class Match(range: Power) {
  def marry(candidates: List[Individual]): List[(Individual, Individual)] = {
    val toWork = Chain.fromSeq(
      candidates
        .sortBy(c => -c.power)
    )

    @tailrec
    def loop(
      people: Chain[Individual],
      matches: List[(Individual, Individual)]
    ): List[(Individual, Individual)] = {
      people match {
        case head ==: second ==: tail
            if ((head.power - second.power) <= range) =>
          loop(tail, (head, second) :: matches)
        case _ ==: tail => loop(tail, matches)
        case Chain.nil  => matches
      }
    }

    loop(toWork, Nil)
  }
}
