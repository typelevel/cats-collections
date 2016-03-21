package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitraryDList {
  import dogs.tests.arbitrary.list._

  def fromLL[A](l1: List[A], l2: List[A], l3: List[A], l4: List[A]): DList[A] = 
    DList(l1) ++ DList(l2) ++ DList(l3) ++ DList(l4)


  def dlistGen[A](implicit A: Arbitrary[A]): Gen[DList[A]] =
    for {
      l1 <- listGen[A]
      l2 <- listGen[A]
      l3 <- listGen[A]
      l4 <- listGen[A]
    } yield(fromLL(l1,l2,l3,l4))

  implicit def arbitraryDList[A: Arbitrary]: Arbitrary[DList[A]] = Arbitrary(dlistGen)
}
