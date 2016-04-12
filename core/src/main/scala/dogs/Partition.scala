/**
 * Created by nperez on 4/12/16.
 */

package dogs


import Predef._
import simulacrum.typeclass
import scala.{inline,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.{tailrec}
import dogs.syntax.birds._
import cats._


@typeclass trait Partition[A] {

  /**
   * Returns the equivalent classes based on the function (property) f of the set
   */
  def partition[B](set: Set[A])(f: A => B)(implicit order: Order[B]): List[(B, List[A])]
}

object Partition {
  def apply[A](implicit order: Order[A]): Partition[A] = new EquivalentClass[A]

  final class EquivalentClass[A](implicit orderA: Order[A]) extends Partition [A] {
    /**
      * Returns the equivalent classes based on the function (property) f of the set
     */
    override def partition[B](set: Set[A])(f: (A) => B)(implicit order: Order[B]): List[(B, List[A])] = {
      def loop(aList: List[A], map: Map[B, List[A]]): List[(B, List[A])] = aList match {
        case El()       =>  map.toList
        case Nel(h, t)  =>  {

          val k = f(h)
          val v = map.get(k).getOrElse(El[A])
          val m = map.updateAppend(k,  h :: v)

          loop(t, m)
        }
      }

      loop(set.toList(), Map.empty)
        .map { case (k, l) => (k, Set.fromList(l).toList()) }
    }
  }
}
