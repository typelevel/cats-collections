package dogs
package tests

import Predef._

import catalysts.Platform

import cats._

import org.scalatest.{FunSuite, PropSpec, Matchers}
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.typelevel.discipline.scalatest.Discipline

import org.scalacheck.{Arbitrary, Gen}

import scala.util.{Failure, Success, Try}

trait TestSettings extends Configuration with Matchers {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm)
      if(scala.sys.env.get("TRAVIS").isDefined)
        PropertyCheckConfiguration(minSuccessful = 1, maxDiscardedFactor = 500F, minSize = 0, sizeRange = 3, workers = 4)
      else
        PropertyCheckConfiguration(minSuccessful = 100, maxDiscardedFactor = 500F, minSize = 0, sizeRange = 100, workers = 4)
    else
      PropertyCheckConfiguration(minSuccessful = 1, maxDiscardedFactor = 10F, minSize = 0, sizeRange =  3, workers = 4)

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm)
      if(scala.sys.env.get("TRAVIS").isDefined)
        PropertyCheckConfiguration(minSuccessful = 1, maxDiscardedFactor = 500F, minSize = 0, sizeRange = 3, workers = 4)
      else
        PropertyCheckConfiguration(minSuccessful = 10, maxDiscardedFactor = 500F, minSize = 0, sizeRange = 100, workers = 4)
    else
      PropertyCheckConfiguration(minSuccessful = 1, maxDiscardedFactor = 10F, minSize = 0, sizeRange = 3, workers = 4)
}

trait DogsSuite extends FunSuite
    with Matchers
    with DogMatcher
    with GeneratorDrivenPropertyChecks
    with TestSettings
    with Discipline
    with StrictDogsEquality {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  implicit def eqTuple2[A: Eq, B: Eq]: Eq[(A,B)] = new Eq[(A,B)] {
    def eqv(l: (A,B), r: (A,B)) =
      Eq[A].eqv(l._1, r._1) &&
      Eq[B].eqv(l._2, r._2)
  }

  implicit def eqTuple3[A: Eq, B: Eq, C: Eq]: Eq[(A,B,C)] = new Eq[(A,B,C)] {
    def eqv(l: (A,B,C), r: (A,B,C)) =
      Eq[A].eqv(l._1, r._1) &&
      Eq[B].eqv(l._2, r._2) &&
      Eq[C].eqv(l._3,r._3)
  }
}

trait SlowDogsSuite extends DogsSuite {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration
}
