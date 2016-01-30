package dogs
package tests

import cats._
import cats.syntax.all._
//import cats.laws.discipline.ArbitraryK
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

  implicit def arbitraryMaybeT[F[_], A](implicit F: Arbitrary[F[Maybe[A]]]): Arbitrary[MaybeT[F, A]] =
    Arbitrary(F.arbitrary.map(MaybeT(_)))
}
