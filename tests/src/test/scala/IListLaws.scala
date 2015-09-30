package dogs
package tests

import cats._
import cats.std.all._
import cats.laws.discipline.{ArbitraryK,SerializableTests,TraverseTests}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import scala.{Int,Option}
import scala.Predef._
import scala.collection.immutable.List
import org.scalacheck._
import org.scalacheck.Arbitrary._

class IListLaws extends FunSuite with Discipline {
  import IList._

  implicit val ilistArbitraryK: ArbitraryK[IList] = new ArbitraryK[IList] {
      def synthesize[A: Arbitrary]: Arbitrary[IList[A]] = arbitrayIList
  }

  implicit def arbitrayIList[A : Arbitrary]: Arbitrary[IList[A]] = Arbitrary(arbitrary[List[A]].map(IList.fromList))

  implicit def monoid[A]: Monoid[IList[A]] = new Monoid[IList[A]] {
    override def empty = INil()
    override def combine(x: IList[A], y: IList[A]) = x ++ y
  }

  checkAll("IList[Int]", algebra.laws.GroupLaws[IList[Int]].monoid)
  checkAll("IList[Int] with Option", TraverseTests[IList].traverse[Int,Int,Int,Int,Option,Option])
  checkAll("IList[Int]", SerializableTests.serializable(Applicative[IList[?]]))
}
