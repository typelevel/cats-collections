package dogs
package tests

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import scala.collection.immutable.List
import scala.Predef._

trait ArbitraryLst {

  implicit def arbitraryLst[A: Arbitrary]: Arbitrary[Lst[A]] =
    Arbitrary(arbitrary[List[A]].map(Lst.fromIterable _))

  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[Nel[A]] = Arbitrary(
    for {
      a <- arbitrary[A]
      as <- arbitrary[Lst[A]]
    } yield(Nel(a, as))
  )

}
