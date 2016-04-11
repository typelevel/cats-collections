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


@typeclass trait Partition[A] {
  def partition(predicate: A => Boolean): (List[A], List[A])
}

object Partition {

  def apply[A](aList: List[A]): Partition[A] = new Partition[A] {
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
