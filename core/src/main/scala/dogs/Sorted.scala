package dogs

import Predef._
import simulacrum.typeclass
import scala.{inline,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.{tailrec}
import dogs.syntax.birds._
import cats._

/**
  * Created by anicolaspp on 4/10/16.
  */

@typeclass trait Sorted[A] {
  def sorted(implicit order: Order[A]): List[A]
}

object Sorted {

  def apply[A](aList: List[A])(implicit order: Order[A]): Sorted[A] = new QuickSorted[A](aList)

  def quickSort[A](aList: List[A])(implicit order: Order[A]): Sorted[A] = new QuickSorted[A](aList)

  def heapSort[A](aList: List[A])(implicit order: Order[A]): Sorted[A] = new HeapSorted[A](aList)

  sealed class QuickSorted[A](aList: List[A]) extends Sorted[A] {
    override def sorted(implicit order: Order[A]): List[A] = {
      def quickSort(xs: List[A], order: Order[A]): List[A] = xs match {
        case El()       => El[A]
        case Nel(h, t)  => quickSort(t.filter(i => order.lt(i, h)), order) ::: (h :: quickSort(t.filter(i => order.gteqv(i, h)), order))
      }

      quickSort(aList, order)
    }
  }

  sealed class HeapSorted[A](aList: List[A]) extends Sorted[A]{
    override def sorted(implicit order: Order[A]): List[A] = {
      val heap = Heap.heapify(aList)

      heap.toList()
    }
  }
}
