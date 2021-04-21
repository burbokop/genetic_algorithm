package genetic

case class DiophantusEquation[T](coefficients: Array[T], result: T) {
  def fitness(chromosome: Chromosome[T])(implicit numeric: Numeric[T]): T =
    numeric.minus((0 until Math.min(chromosome.genes.length, coefficients.length))
      .map(i => numeric.times(coefficients(i), chromosome.genes(i)))
      .sum, result)

  override def toString: String = s"[${coefficients.mkString(", ")}], $result"
}
