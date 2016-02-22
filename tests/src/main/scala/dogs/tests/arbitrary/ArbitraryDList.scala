package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitraryDList {
  import dogs.tests.arbitrary.list._

  def fromLL[A](ls: List[List[A]]): DList[A] = 
    ls.foldLeft(DList.empty[A])((dl,l) => dl ++ DList(l))

  def dlistGen[A](implicit A: Arbitrary[A]): Gen[DList[A]] =
    listGen[List[A]].map(fromLL)

  implicit def arbitraryDList[A: Arbitrary]: Arbitrary[DList[A]] = Arbitrary(dlistGen)
}
