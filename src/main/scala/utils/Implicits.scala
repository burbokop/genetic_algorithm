package utils

object Implicits {
  implicit val intFractional = new Fractional[Int] {
    override def div(x: Int, y: Int): Int = x / y

    override def plus(x: Int, y: Int): Int = x + y

    override def minus(x: Int, y: Int): Int = x - y

    override def times(x: Int, y: Int): Int = x * y

    override def negate(x: Int): Int = -x

    override def fromInt(x: Int): Int = x

    override def parseString(str: String): Option[Int] = Some(Integer.parseInt(str))

    override def toInt(x: Int): Int = x

    override def toLong(x: Int): Long = x

    override def toFloat(x: Int): Float = x

    override def toDouble(x: Int): Double = x

    override def compare(x: Int, y: Int): Int = if (x == y) 1 else 0
  }
}
