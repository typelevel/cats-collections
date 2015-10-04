package dogs
package tests

import cats._
import cats.std.all._
import cats.laws.discipline.{ArbitraryK,SerializableTests,TraverseTests,CoflatMapTests,ComonadTests}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import scala.{Int,Option}
import scala.Predef._
import scala.collection.immutable.List
import cats.laws.discipline.EqK

class LstLaws extends FunSuite with Discipline with ArbitraryLst {
  import Lst._

  implicit val nelEQ: EqK[Nel] =
    new EqK[Nel] {
      def synthesize[A: Eq]: Eq[Nel[A]] = implicitly[Eq[Lst[A]]].on(identity)
    }

  implicit def monoid[A : Monoid] = new Monoid[Lst[A]] {
    override def empty = El()
    override def combine(x: Lst[A], y: Lst[A]) = x ::: y
  }

  checkAll("Lst[Int]", algebra.laws.GroupLaws[Lst[Int]].monoid)
  checkAll("Lst[Int] with Option", TraverseTests[Lst].traverse[Int,Int,Int,Int,Option,Option])
  checkAll("Lst[Int]", SerializableTests.serializable(Applicative[Lst[?]]))
  checkAll("Lst[Int]", CoflatMapTests[Lst].coflatMap[Int,Int,Int])
  checkAll("Nel[Int]", ComonadTests[Nel].comonad[Int,Int,Int])
}
