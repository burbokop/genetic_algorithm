package genetic

import scala.annotation.tailrec
import scala.util.Random

trait RandGeg[T] {
  def generate(from: T, to: T): T
}

object RandGeg {
  val random = new Random(System.nanoTime())


  implicit val int = new RandGeg[Int] {
    override def generate(from: Int, to: Int): Int = {
      val rand = random.nextInt()
      if (from >= 0) {
        Math.abs(rand) % (to - from) + from
      } else if(to >= 0) {
        if (rand >= 0) {
          Math.abs(rand) % Math.abs(to - from) + from
        } else {
          -(Math.abs(rand) % Math.abs(to - from)) + from
        }
      } else {
        -(Math.abs(rand) % Math.abs(to - from)) + from
      }
    }
  }

  implicit val double = new RandGeg[Double] {
    override def generate(from: Double, to: Double): Double = random.nextDouble() * (to - from) + from
  }

}
