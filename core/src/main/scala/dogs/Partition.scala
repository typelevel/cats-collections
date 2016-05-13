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


@typeclass trait Partition[M[_]] {

  /**
   * Returns the equivalent classes based on the function (property) f of the set
   */
  def partitions[A: Order, B: Order](xs: M[A])(f: A => B): List[(B, List[A])]
}
