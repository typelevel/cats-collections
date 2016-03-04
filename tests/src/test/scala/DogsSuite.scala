package dogs
package tests

import org.scalatest.{FunSuite, PropSpec, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}

trait DogsSuite extends FunSuite
    with Matchers
    with Configuration
    with GeneratorDrivenPropertyChecks
    with Discipline
    with DogMatcher
