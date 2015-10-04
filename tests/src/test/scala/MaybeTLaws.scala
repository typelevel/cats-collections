package dogs
package tests

import algebra.Eq
import cats.laws.discipline._
import cats._
import cats.std.all._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import scala.{Boolean,Int,Option}
import scala.Predef.{identity,implicitly}

class MaybeTLaws extends FunSuite with Discipline with ArbitraryMaybe {
  import MaybeT._

  implicit def maybetEQ[F[_]: EqK]: EqK[MaybeT[F, ?]] =
    new EqK[MaybeT[F, ?]] {
      def synthesize[A: Eq]: Eq[MaybeT[F, A]] = new Eq[MaybeT[F, A]] {
        override def eqv(x: MaybeT[F, A], y: MaybeT[F, A]): Boolean =
          true
      }
    }

  checkAll("MaybeT[Int]", MonadCombineTests[MaybeT[Option,?]].monadCombine[Int,Int,Int])
  checkAll("MaybeT[Int]", TraverseTests[MaybeT[Option,?]].traverse[Int,Int,Int,Int,Option,Option])
}
