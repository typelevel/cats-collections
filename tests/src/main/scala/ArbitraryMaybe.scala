package dogs
package tests

import cats._
import cats.syntax.all._
import cats.laws.discipline.ArbitraryK
import org.scalacheck._
import org.scalacheck.Arbitrary._
import scala.Boolean

trait ArbitraryMaybe {
  import Maybe._

  implicit val genMonad: Monad[Gen] = new Monad[Gen] {
    override def pure[A](a: A): Gen[A] = Gen.const(a)
    override def flatMap[A,B](fa: Gen[A])(f: A => Gen[B]) = fa flatMap f
  }

  implicit def arbitraryMaybe[A: Arbitrary]: Arbitrary[Maybe[A]] =
    Arbitrary(arbitrary[Boolean].ifM(Gen.const(NotThere()), arbitrary[A].map(There.apply)))

  implicit val maybeArbitraryK: ArbitraryK[Maybe] = new ArbitraryK[Maybe] {
    def synthesize[A: Arbitrary]: Arbitrary[Maybe[A]] = arbitraryMaybe
  }

  implicit def arbitraryMaybeT[F[_], A](implicit F: ArbitraryK[F],
                                        A: Arbitrary[A]): Arbitrary[MaybeT[F, A]] =
    Arbitrary(arbitrary(F.synthesize[Maybe[A]]).map(MaybeT.apply))

  implicit def maybeTArbitraryK[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[MaybeT[F,?]] =
    new ArbitraryK[MaybeT[F,?]] {
      def synthesize[A: Arbitrary]: Arbitrary[MaybeT[F,A]] =
        Arbitrary(arbitrary(F.synthesize[Maybe[A]]).map(MaybeT.apply))
  }
}
