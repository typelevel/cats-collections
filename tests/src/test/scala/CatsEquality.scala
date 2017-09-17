package dogs
package tests

import cats._

import org.scalactic._
import TripleEqualsSupport.AToBEquivalenceConstraint
import TripleEqualsSupport.BToAEquivalenceConstraint

// The code in this file was taken and only slightly modified from
// https://github.com/bvenners/equality-integration-demo
// Thanks for the great examples, Bill!

final class DogsEquivalence[T](T: Eq[T]) extends Equivalence[T] {
  def areEquivalent(a: T, b: T): Boolean = T.eqv(a, b)
}

trait LowPriorityStrictDogsConstraints extends TripleEquals {
  implicit def lowPriorityDogsCanEqual[A, B](implicit B: Eq[B], ev: A <:< B): CanEqual[A, B] =
    new AToBEquivalenceConstraint[A, B](new DogsEquivalence(B), ev)
}

trait StrictDogsEquality extends LowPriorityStrictDogsConstraints {
  override def convertToEqualizer[T](left: T): Equalizer[T] = super.convertToEqualizer[T](left)
  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)
  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): CanEqual[A, B] = super.unconstrainedEquality[A, B]
  implicit def dogsCanEqual[A, B](implicit A: Eq[A], ev: B <:< A): CanEqual[A, B] =
    new BToAEquivalenceConstraint[A, B](new DogsEquivalence(A), ev)
}

object StrictDogsEquality extends StrictDogsEquality
