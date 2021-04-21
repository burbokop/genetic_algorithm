package tasks

import genetic.{Chromosome, DiophantusEquation}
import utils.Implicits._
import org.nspl.{line, _}
import awtrenderer._

import scala.language.postfixOps

object FitnessPlotTask extends Task {
  override def exec(): Unit = {
    val equation = DiophantusEquation[Int](Array(4, 2, 5, 3), 26)
    val chromosomes = (0 until 4).map { _ =>
      Chromosome.generate(0, equation.result / equation.coefficients.length, equation.coefficients.length)
    }
    println(s"equation: $equation")

    val (result, averageFitness) = Chromosome.exec(
      initialChromosomes = chromosomes,
      fitnessFunction = equation.fitness,
      mutationDelta = 1,
      mutationChance = 0.009,
      iterationLimit = 100000
    )

    println(s"result: $result")

    val plot = xyplot(
      averageFitness.indices.map(_.toDouble) ->
        averageFitness.map(_.toDouble) ->
        line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 255))
    )(
      ylab = "average fitness",
      xlab = "iteration",
      main = "average fitness on each iteration",
    )

    show(plot)
  }
}
