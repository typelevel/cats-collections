package dogs
package tests

import algebra.Eq
import cats.laws.discipline._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.implicits._
import scala.{Boolean,Int,Option}
import scala.Predef.{identity,implicitly}

class MaybeTLaws extends FunSuite with Discipline with ArbitraryMaybe {
  import MaybeT._

  checkAll("MaybeT[Int]", MonadCombineTests[MaybeT[Option,?]].monadCombine[Int,Int,Int])
  checkAll("MaybeT[Int]", TraverseTests[MaybeT[Option,?]].traverse[Int,Int,Int,Int,Option,Option])
}
