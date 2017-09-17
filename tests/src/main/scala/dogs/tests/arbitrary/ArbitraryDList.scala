package dogs
package tests.arbitrary

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

trait ArbitraryDList {
  implicit def arbitraryDList[A: Arbitrary]: Arbitrary[DList[A]] = Arbitrary(arbitrary[List[A]].map(DList(_)))
}
