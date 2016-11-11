package dogs
package tests.arbitrary

import cats.Foldable

import org.scalacheck._
import rng.Seed

trait CogenInstances {
  implicit def cogenFoldable[F[_], A](implicit F: Foldable[F], A: Cogen[A]): Cogen[F[A]] =
    Cogen[F[A]]((seed: Seed, fa: F[A]) => F.foldLeft(fa, seed)(A.perturb))
}
