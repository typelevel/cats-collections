package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitraryDList {
  import dogs.tests.arbitrary.list._

  def fromLL[A](l1: List[A], l2: List[A]): DList[A] = 
    DList(l1) ++ DList(l2)


  def dlistGen[A](implicit A: Arbitrary[A]): Gen[DList[A]] =
    for {
      l1 <- listGen[A]
      l2 <- listGen[A]
    } yield(fromLL(l1,l2))

  implicit def arbitraryDList[A: Arbitrary]: Arbitrary[DList[A]] = Arbitrary(dlistGen)
}
