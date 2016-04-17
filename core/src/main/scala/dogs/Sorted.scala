/**
 * Created by anicolaspp on 4/10/16.
 */

package dogs

import Predef._
import simulacrum.typeclass
import scala.{inline,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.{tailrec}
import dogs.syntax.birds._
import cats._


@typeclass trait Sorted[M[_]] {
  def sorted[A](xs: M[A])(implicit order: Order[A]): List[A]
}

object Sorted {

  def apply: Sorted[List] = new QuickSorted

  def quickSort: Sorted[List] = new QuickSorted

  def heapSort: Sorted[List] = new HeapSorted

  sealed class QuickSorted extends Sorted[List] {
    override def sorted[A](xs: List[A])(implicit order: Order[A]): List[A] = {
      def quickSort(xs: List[A], order: Order[A]): List[A] = xs match {
        case El()       => El[A]
        case Nel(h, t)  => {
          val (l, r) = t.partition(order.lt(_,h))

          quickSort(l, order) ::: (h :: quickSort(r, order))
        }
      }

      quickSort(xs, order)
    }
  }

  sealed class HeapSorted extends Sorted[List]{
    override def sorted[A](xs: List[A])(implicit order: Order[A]): List[A] = {
      val heap = Heap.heapify(xs)

      heap.toList()
    }
  }
}

