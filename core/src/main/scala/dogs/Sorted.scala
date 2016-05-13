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


  private trait BiPartition[A] {
      def partition(predicate: A => Boolean): (List[A], List[A])
    }

  private object BiPartition {

      def apply[A](aList: List[A]): BiPartition[A] = new BiPartition[A] {
        override def partition(f: (A) => Boolean): (List[A], List[A]) = {
          var lbuilder = new ListBuilder[A]
          var rbuilder = new ListBuilder[A]

          aList.foreach { x =>
            if (f(x)) lbuilder = lbuilder += x
            else
              rbuilder = rbuilder += x
          }

          (lbuilder.run, rbuilder.run)
        }
      }
    }

  def apply: Sorted[List] = new QuickSorted

  def quickSort: Sorted[List] = new QuickSorted

  def heapSort: Sorted[List] = new HeapSorted

  sealed class QuickSorted extends Sorted[List] {
    override def sorted[A](xs: List[A])(implicit order: Order[A]): List[A] = {
      def quickSort(xs: List[A], order: Order[A]): List[A] = xs match {
        case El()       => El[A]
        case Nel(h, t)  => {
          val (l, r) = BiPartition(t).partition(order.lt(_,h))

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

