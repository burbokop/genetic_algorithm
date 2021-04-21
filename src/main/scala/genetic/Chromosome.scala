package genetic

import genetic.IndexedSeqImplicits._

import scala.annotation.tailrec
import scala.reflect.ClassTag

class Chromosome[T:ClassTag](g: Array[T]) {
  def genes = g
  def this(f: Int => T, count: Int) = this((0 until count).map(f).toArray)

  override def toString: String = s"[${genes.mkString(", ")}]"

  def recombineWith(that: Chromosome[T], point: Int): (Chromosome[T], Chromosome[T]) =
    if (point < genes.length && point < that.genes.length) (
      Chromosome(genes.take(point + 1) ++ that.genes.takeRight(that.genes.length - point - 1)),
      Chromosome(that.genes.take(point + 1) ++ genes.takeRight(genes.length - point - 1))
    )
    else (
      this,
      that
    )

  def recombineWith(that: Chromosome[T]): (Chromosome[T], Chromosome[T]) =
    recombineWith(that, RandGeg.int.generate(0, Math.min(genes.length, that.genes.length) - 1))

  def mutate(delta: T, chance: Double)(implicit numeric: Numeric[T]): Chromosome[T] =
    Chromosome(genes.indices.map { i =>
      if(RandGeg.double.generate(0, 1) < chance) {
        if (RandGeg.int.generate(0, 2) == 1) numeric.plus(genes(i), delta)
        else numeric.minus(genes(i), delta)
      } else {
        genes(i)
      }
    }.toArray)
}


object Chromosome {
  def apply[T:ClassTag](g: Array[T]): Chromosome[T] = new Chromosome(g)
  def apply[T:ClassTag](f: Int => T, count: Int): Chromosome[T] = new Chromosome(f, count)

  def generate[T:ClassTag](from: T, to: T, count: Int)
                          (implicit randGeg: RandGeg[T]) =
    Chromosome(_ => randGeg.generate(from, to), count)

  def normalizeFitnessValues[T:ClassTag](fitnessValues: IndexedSeq[T])(implicit fractional: Fractional[T]): IndexedSeq[Double] = {
    val coefficients = fitnessValues.map(1 / fractional.toDouble(_))
    val sum = coefficients.sum
    coefficients.map(_ / sum)
  }

  def selectWithFitnessValues[T:ClassTag](chromosomes: IndexedSeq[Chromosome[T]], fitnessFunction: Chromosome[T] => T)
                                         (implicit fractional: Fractional[T]) = {
    val fitness = chromosomes.map(fitnessFunction(_))
    val nfv = normalizeFitnessValues(fitness)
      .map(Math.abs(_))
      .sorted
      .mapWithLast((c, l) => l.map(c + _).getOrElse(c))
    (
      (0 until chromosomes.length).map(_ => chromosomes(nfv.indexWhere(_ > RandGeg.double.generate(0, 1)))),
      fitness
    )
  }

  def select[T:ClassTag](chromosomes: IndexedSeq[Chromosome[T]], fitnessFunction: Chromosome[T] => T)
                        (implicit fractional: Fractional[T]) =
    selectWithFitnessValues(chromosomes, fitnessFunction)._1


  def exec[T:ClassTag](
                        initialChromosomes: IndexedSeq[Chromosome[T]],
                        fitnessFunction: Chromosome[T] => T,
                        mutationDelta: T,
                        mutationChance: Double,
                        iterationLimit: Int
                      )
                      (implicit fractional: Fractional[T]): (Option[Chromosome[T]], IndexedSeq[T]) = {
    @tailrec
    def iterator(chromosomes: IndexedSeq[Chromosome[T]], i: Int, averageFitness: IndexedSeq[T]): (Option[Chromosome[T]], IndexedSeq[T]) = {
      val found = chromosomes.find(fitnessFunction(_) == 0)
      if(found.isDefined) {
        val caf = fractional.div(chromosomes.map(fitnessFunction(_)).sum, fractional.fromInt(chromosomes.length))
        (Some(found.get), averageFitness ++ IndexedSeq(caf))
      } else if(i > iterationLimit || chromosomes.length == 0) {
        val caf = fractional.div(chromosomes.map(fitnessFunction(_)).sum, fractional.fromInt(chromosomes.length))
        (None, averageFitness ++ IndexedSeq(caf))
      } else {
        val (selected, fitness) = Chromosome.selectWithFitnessValues(chromosomes, fitnessFunction)
        val children = selected.mapRandomPairs((a, b) => a.recombineWith(b))
        val mutatedChildren = children.map(_.mutate(mutationDelta, mutationChance))
        iterator(mutatedChildren, i + 1, averageFitness ++ IndexedSeq(fractional.div(fitness.sum, fractional.fromInt(fitness.length))))
      }
    }
    iterator(initialChromosomes, 0, IndexedSeq())
  }
}
