package dogs
package tests

import dogs.Predef._
import java.lang.String
import scala.{Function,Int,Unit}
import scala.collection.immutable.List
import org.scalacheck._
import org.scalacheck.Prop._

object BedazzleSpec extends Properties("Bedazzle") {
  property("some") = secure {
    import syntax.option._
    1.some == Some(1)
  }
  property("some") = secure {
    import syntax.all._
    none[String] == Option.apply[String](null)
  }

  property("i fixed ur inference") = secure {
    import syntax.list._
    List(1,2,3).foldRight(nil[Int])(_ :: _).sum == 6
  }

  property("kestrel aka unsafeTap") = secure {
    import syntax.birds._
    
    var called = false
    val call: String => Unit = Function.const(called = true)
    val a = "xyzzy"
    val b = a <| call

    a == b && called
  }

  property("thrush") = secure {
    import syntax.all._

    val a = 1
    val f: Int => String = _.toString
    val b = a |> f

    b == "1"
  }

  property("$") = secure {
    import syntax.all._

    val a = 1
    val f: Int => String = _.toString
    val b = a $ f

    b == "1"
  }
}

