package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitraryStreaming {
  import cats.laws.discipline.arbitrary._

  def streamingGen[A:Arbitrary](maxDepth: Int): Gen[Streaming[A]] =
    if (maxDepth <= 1)
      Gen.const(Streaming.empty[A])
    else {
      // the arbitrary instance for the next layer of the stream
      implicit val A = Arbitrary(streamingGen[A](maxDepth - 1))
      Gen.frequency(
        // Empty
        1 -> Gen.const(Streaming.empty[A]),
        // Wait
        2 -> arbitrary[Eval[Streaming[A]]].map(Streaming.wait(_)),
        // Cons
        6 -> (for {
          a <- arbitrary[A]
          tail <- arbitrary[Eval[Streaming[A]]]
        } yield Streaming.cons(a, tail)))
    }

  implicit def streamingArbitrary[A:Arbitrary]: Arbitrary[Streaming[A]] =
    Arbitrary(streamingGen[A](8))
}

