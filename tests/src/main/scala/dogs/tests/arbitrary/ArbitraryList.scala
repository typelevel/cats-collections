package dogs
package tests.arbitrary

import Predef._
import org.scalacheck.{Gen, Arbitrary, Shrink}, Arbitrary.arbitrary
import org.scalacheck.util.Buildable

trait ArbitraryList {
  implicit def listBuildable[A]: Buildable[A,List[A]] = new Buildable[A,List[A]] {
    override def builder = new ListBuilder
  }

  def listGen[A](implicit arbA: Arbitrary[A]): Gen[List[A]] =
    Gen.containerOf[List,A](arbA.arbitrary)(implicitly, l => l.toScalaList)

  implicit def arbitraryList[A: Arbitrary]: Arbitrary[List[A]] =
    Arbitrary(listGen)

  def nelGen[A](implicit arbA: Arbitrary[A]): Gen[Nel[A]] =
    for {
      a <- arbitrary[A]
      as <- listGen[A]
    } yield Nel(a,as)

  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[Nel[A]] =
    Arbitrary(nelGen)


  implicit def shrinkList[A]: Shrink[List[A]] = Shrink { l =>
    import scala.collection.immutable.{List => SList}
    implicitly[Shrink[SList[A]]].shrink(l.toScalaList).map(List.fromIterable)
  }
}
