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


@typeclass trait Sorted[A] {
  def sorted(xs: List[A]): List[A]
}

object Sorted {

  def apply[A](implicit order: Order[A]): Sorted[A] = new QuickSorted[A]

  def quickSort[A](implicit order: Order[A]): Sorted[A] = new QuickSorted[A]

  def heapSort[A](implicit order: Order[A]): Sorted[A] = new HeapSorted[A]

  sealed class QuickSorted[A](implicit order: Order[A]) extends Sorted[A] {
    override def sorted(xs: List[A]): List[A] = {
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

  sealed class HeapSorted[A](implicit order: Order[A]) extends Sorted[A]{
    override def sorted(xs: List[A]): List[A] = {
      val heap = Heap.heapify(xs)

      heap.toList()
    }
  }
}

