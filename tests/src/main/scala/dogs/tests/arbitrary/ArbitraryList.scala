package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary.arbitrary

trait ArbitraryList {
  def listGen[A](implicit arbA: Arbitrary[A]): Gen[List[A]] =
    Gen.listOf[A](arbitrary(arbA)).map(_.foldRight[List[A]](El())(Nel.apply))
                              
  implicit def arbitraryList[A: Arbitrary]: Arbitrary[List[A]] =
    Arbitrary(listGen)

  def nelGen[A](implicit arbA: Arbitrary[A]): Gen[Nel[A]] =
    for {
      a <- arbitrary[A]
      as <- listGen[A]
    } yield Nel(a,as)

  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[Nel[A]] =
    Arbitrary(nelGen)

}
