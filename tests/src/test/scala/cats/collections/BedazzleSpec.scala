package cats.collections
package tests

import cats.tests.CatsSuite
import scala.Function

class BedazzleSpec extends CatsSuite {
  test("kestrel aka unsafeTap"){
    import syntax.birds._

    var called = false
    val call: String => Unit = { x => called = true }
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

