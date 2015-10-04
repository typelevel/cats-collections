package dogs
package tests

import cats._
import cats.implicits._

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck._
import org.scalacheck.Prop.{forAll,secure}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.{FunSuite, PropSpec, Matchers}

import scala.Boolean
import scala.Int
import java.lang.String

object MaybeTest extends Properties("LstTest") with ArbitraryMaybe {
  import Maybe._

  property("Empty is less than anything else") = forAll { (x: Maybe[Int]) =>
    Order[Maybe[Int]].gteqv(x, Maybe.notThere)
  }

  property("Preserved through Option") = forAll { (x: Maybe[Int]) =>
    Maybe.fromOption(x.toOption) === x
  }

  property("there toFailure is failure") = forAll { (x: Int, s: String) => there(x).toInvalid(s).isInvalid }

    property("notThere toFailure is success") = forAll { s: String => notThere.toInvalid(s).isValid }

    property("there toSuccess is success") = forAll { (x: Int, s: String) => there(x).toValid(s).isValid }

    property("notThere toSuccess is failure") = forAll { s: String => notThere.toValid(s).isInvalid }

    property("there toLeft is left") = forAll { (x: Int, s: String) => there(x).toLeft(s).isLeft }

    property("notThere toLeft is right") = forAll { s: String => notThere.toLeft(s).isRight }

    property("there toRight is right") = forAll { (x: Int, s: String) => there(x).toRight(s).isRight }

    property("notThere toRight is left") = forAll { s: String => notThere.toRight(s).isLeft }

    property("there isThere") = forAll { x: Int => there(x).isThere }

    property("there isn't notThere") = forAll { x: Int => !there(x).isNotThere }

    property("notThere is notThere") = secure(notThere.isNotThere)

    property("notThere isn't there") = secure(!notThere.isThere)

    property("there to option is some") = forAll { x: Int => there(x).toOption.isDefined }

    property("notThere to option is none") = secure(notThere.toOption.isEmpty)

    property("there orElse is there") = forAll { (x: Int, m: Maybe[Int]) => there(x).orElse(m).isThere }

    property("fromNullable(null) is NotThere") = secure {
    val s: String = null
    Maybe.fromNullable(s).isNotThere
  }

    property("fromNullable(notNull) is there") = forAll { (s: String) => Maybe.fromNullable(s) === there(s) }

  object instances {
    def equal[A: Eq] = Eq[Maybe[A]]
    def order[A: Order] = Order[Maybe[A]]
    def semigroup[A: Semigroup] = Monoid[Maybe[A]]
    def monad[A] = Monad[Maybe]

    // checking absence of ambiguity
    def eqv[A: Order] = Eq[Maybe[A]]
  }
}
