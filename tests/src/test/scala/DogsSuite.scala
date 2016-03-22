package dogs
package tests

import catalysts.Platform

import cats._
import org.scalactic.anyvals.{PosZDouble, PosInt}
import org.scalatest.{FunSuite, PropSpec, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}

trait TestSettings extends Configuration with Matchers {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(100) else PosInt(1),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0))

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm) checkConfiguration
    else PropertyCheckConfig(maxSize = 1, minSuccessful = 1)
}

trait DogsSuite extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with TestSettings
    with Discipline
    with DogMatcher
    with StrictDogsEquality {

  implicit def eqTuple3[A: Eq, B: Eq, C: Eq](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A,B,C)] = new Eq[(A,B,C)] {
    def eqv(l: (A,B,C), r: (A,B,C)) =
      A.eqv(l._1, r._1) &&
      B.eqv(l._2, r._2) &&
      C.eqv(l._3, r._3)
  }

  implicit def eqTuple2[A: Eq, B: Eq](implicit A: Eq[A], B: Eq[B]): Eq[(A,B)] = new Eq[(A,B)] {
    def eqv(l: (A,B), r: (A,B)) =
      A.eqv(l._1, r._1) &&
      B.eqv(l._2, r._2)
  }
}

trait SlowDogsSuite extends DogsSuite {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration
}
