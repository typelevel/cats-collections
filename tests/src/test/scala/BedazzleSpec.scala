package dogs
package tests

import dogs.Predef._

import scala.Function
import scala.collection.immutable.List

class BedazzleSpec extends DogsSuite {

  test("some1") {
    import syntax.option._
    1.some shouldBe Some(1)
  }

  test("some2") {
    import syntax.all._
    none[String] shouldBe Option.apply[String](null)
  }

  test("i fixed ur inference"){
    import syntax.list._
    List(1,2,3).foldRight(nil[Int])(_ :: _).sum shouldBe 6
  }

  test("kestrel aka unsafeTap"){
    import syntax.birds._

    var called = false
    val call: String => Unit = Function.const(called = true)
    val a = "xyzzy"
    val b = a <| call

    (a == b) shouldBe called
  }

  test("thrush"){
    import syntax.all._

    val a = 1
    val f: Int => String = _.toString
    val b = a |> f

    b shouldBe "1"
  }

  test("$"){
    import syntax.all._

    val a = 1
    val f: Int => String = _.toString
    val b = a $ f

    b shouldBe "1"
  }
}

