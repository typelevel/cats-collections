package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import Gen.oneOf

trait ArbitraryEval {
  def genEval[A,B](implicit A: Arbitrary[A]): Gen[Eval[A]] =
    Gen.oneOf(A.arbitrary.map(Eval.now(_)),
              A.arbitrary.map(Eval.later(_)),
              A.arbitrary.map(Eval.always(_)))

  implicit def arbitraryEval[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(genEval)
}
