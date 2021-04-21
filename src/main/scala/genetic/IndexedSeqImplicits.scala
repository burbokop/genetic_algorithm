package genetic

import scala.annotation.tailrec
import scala.reflect.ClassTag


object IndexedSeqImplicits {
  implicit def indexedSeqImplicits[T:ClassTag](array: IndexedSeq[T]) = new IndexedSeqImplicits[T](array)
}

class IndexedSeqImplicits[T:ClassTag](seq: IndexedSeq[T]) {
  def mapWithLast(f: (T, Option[T]) => T): IndexedSeq[T] = {
    @tailrec
    def iterator(seq: IndexedSeq[T], i: Int): IndexedSeq[T] = {
      if(i < seq.length) {
        val newVal = if (i > 0) {
          f(seq(i), Some(seq(i - 1)))
        } else {
          f(seq(i), None)
        }
        iterator(seq.patch(i, Array(newVal), 1), i + 1)
      } else {
        seq
      }
    }
    iterator(seq, 0)
  }

  def removed(index: Int) = seq.take(index) ++ seq.drop(index)

  def mapRandomPairs(f: (T, T) => (T, T)) = {
    seq
      .sortBy(_ => RandGeg.int.generate(0, 2) == 1)
      .grouped(2).map{ pair =>
      if(pair.length == 2) {
        val tup = f(pair(0), pair(1))
        IndexedSeq(tup._1, tup._2)
      } else {
        IndexedSeq()
      }
    }
      .reduce((a, b) => a ++ b)
  }
}
