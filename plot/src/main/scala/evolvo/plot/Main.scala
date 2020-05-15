package evolvo
package plot

import com.cibo.evilplot._
import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import scalaview.Utils._
import cats.implicits._

object Main {
  def main(args: Array[String]): Unit = {

    val parallelization = 12

    val initial = Society.typical(500000, parallelization)

    val societyImg = (society: Society, gen: Int) => {
      println(show"""
                     |
                     |===================Generation $gen =================
                     |$society
                     |
                     |""".stripMargin)
      val data = society.allPowers.map(_.toDouble)

      val img = Histogram(data, bins = 50)
        .title(s"Generation $gen")
        .standard()
        .render()
        .asBufferedImage

      (biResize(img, 1000, 800))
    }
    val stream = Stream
      .iterate(initial)(_.parEvolve(parallelization))
      .take(30)
      .zipWithIndex

    val run = scalaview.SwingImageViewer(
      stream.map(societyImg.tupled),
      timerDelay = 1000
    )

  }

}
