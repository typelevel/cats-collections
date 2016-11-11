package dogs
package tests.arbitrary

import org.scalacheck.Arbitrary

trait ArbitraryDList {
  import dogs.tests.arbitrary.list._

  implicit def arbitraryDList[A: Arbitrary]: Arbitrary[DList[A]] = Arbitrary(listGen[A].map(DList(_)))
}
