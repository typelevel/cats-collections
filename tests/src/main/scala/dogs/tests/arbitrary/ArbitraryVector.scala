package dogs
package tests.arbitrary

import Predef._
import org.scalacheck.{Gen, Arbitrary, Shrink}, Arbitrary.arbitrary

trait ArbitraryVector {
  def vectorGen[A](implicit arbA: Arbitrary[A]): Gen[Vector[A]] =
    Gen.listOf[A](arbitrary(arbA)).map(l => Vector(l:_*))

  implicit def arbitraryVector[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(vectorGen)

  implicit def shrinkVector[A]: Shrink[Vector[A]] = Shrink { l =>
    import scala.collection.immutable.{Vector => SVector}
    implicitly[Shrink[SVector[A]]].shrink(l.toScalaVector).map(v => Vector(v:_*))
  }
}
