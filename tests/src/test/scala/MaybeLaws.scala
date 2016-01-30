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

  checkAll("Maybe[Int]", MonadCombineTests[Maybe].monadCombine[Int,Int,Int])
  checkAll("Maybe[Int]", TraverseTests[Maybe].traverse[Int,Int,Int,Int,Option,Option])
  checkAll("Maybe[Int]", CoflatMapTests[Maybe].coflatMap[Int,Int,Int])
}
