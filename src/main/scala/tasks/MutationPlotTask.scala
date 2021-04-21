package tasks

import genetic.{Chromosome, DiophantusEquation}
import utils.Implicits._
import org.nspl.{line, _}
import awtrenderer._

import scala.language.postfixOps

object MutationPlotTask extends Task {
  override def exec(): Unit = {
    val equation = DiophantusEquation[Int](Array(4, 2, 5, 3), 26)

    val result = (0 to 100).map(_.toDouble / 100).map { mutationChance =>
      println(s"test mutation: $mutationChance")
      val tryCount = 10
      val tup = (0 until tryCount).map { _ =>
        val chromosomes = (0 until 4).map { _ =>
          Chromosome.generate(0, equation.result / equation.coefficients.length, equation.coefficients.length)
        }
        val (_, averageFitness) = Chromosome.exec(
          initialChromosomes = chromosomes,
          fitnessFunction = equation.fitness,
          mutationDelta = 1,
          mutationChance = 0.009,
          iterationLimit = 100000
        )
        (mutationChance, averageFitness.length)
      }
        .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
      (tup._1 / tryCount, tup._2 / tryCount)
    }


    val mutations = result.map(_._1)
    val countOfIterations = result.map(_._2).map(_.toDouble)

    val plot = xyplot(
      mutations -> countOfIterations -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 255)),
    )(
      ylab = "count of iterations",
      xlab = "mutation chance",
      main = "count of iterations depends on mutation chance",
    )

    show(plot)
  }
}
