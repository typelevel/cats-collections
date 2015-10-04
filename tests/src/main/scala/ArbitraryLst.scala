package dogs
package tests

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import cats.laws.discipline.ArbitraryK
import scala.collection.immutable.List
import scala.Predef._

trait ArbitraryLst {

  implicit def arbitraryLst[A: Arbitrary]: Arbitrary[Lst[A]] =
    Arbitrary(arbitrary[List[A]].map(Lst.fromIterable _))

  implicit val lstArbitraryK: ArbitraryK[Lst] = new ArbitraryK[Lst] {
    def synthesize[A: Arbitrary]: Arbitrary[Lst[A]] = arbitraryLst
  }

  implicit val NelArbitraryK: ArbitraryK[Nel] = new ArbitraryK[Nel] {
    def synthesize[A: Arbitrary]: Arbitrary[Nel[A]] = Arbitrary(for {
      a <- arbitrary[A]
      as <- arbitrary[Lst[A]]
    } yield(Nel(a, as)))
  }
}
