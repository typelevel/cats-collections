package dogs
package tests

import algebra.Eq
import cats.laws.discipline._
import cats.std.all._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import scala.{Boolean,Int,Option}
import scala.Predef.{identity,implicitly}

class MaybeLaws extends FunSuite with Discipline with ArbitraryMaybe {
  import Maybe._

  implicit val maybeEQ: EqK[Maybe] =
    new EqK[Maybe] {
      def synthesize[A: Eq]: Eq[Maybe[A]] = new Eq[Maybe[A]] {
        override def eqv(x: Maybe[A], y: Maybe[A]): Boolean = (x,y) match {
          case (NotThere(), NotThere()) => true
          case (There(xx), There(yy)) => Eq[A].eqv(xx,yy)
          case _ => false
        }
      }
    }

  checkAll("Maybe[Int]", MonadCombineTests[Maybe].monadCombine[Int,Int,Int])
  checkAll("Maybe[Int]", TraverseTests[Maybe].traverse[Int,Int,Int,Int,Option,Option])
  checkAll("Maybe[Int]", CoflatMapTests[Maybe].coflatMap[Int,Int,Int])
}
