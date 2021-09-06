package cats

import scala.annotation.tailrec

package object collections {

  @deprecated("ISet has been renamed to Predicate.", "cats-collections 0.8.0")
  type ISet[K] = Predicate[K]
  @deprecated("ISet has been renamed to Predicate.", "cats-collections 0.8.0")
  val ISet = Predicate

  @tailrec private[cats] def iteratorEq[A: Eq](i1: Iterator[A], i2: Iterator[A]): Boolean = {
    val i1N = i1.hasNext
    val i2N = i2.hasNext
    if (!i1N && !i2N)
      true
    else if (i1N != i2N)
      false
    else
      Eq[A].eqv(i1.next(), i2.next()) && iteratorEq(i1, i2)
  }

}
