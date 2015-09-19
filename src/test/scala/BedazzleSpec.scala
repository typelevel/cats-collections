package dogs

import org.scalacheck._
import org.scalacheck.Prop._

object BedazzleSpec extends Properties("Bedazzle") {
  property("some") = secure {
    import bedazzle.option._
    1.some == Some(1)
  }
  property("some") = secure {
    import bedazzle.all._
    none[String] == Option.apply[String](null)
  }

  property("i fixed ur inference") = secure {
    import bedazzle.list._
    List(1,2,3).foldRight(nil[Int])(_ :: _).sum == 6
  }

  property("left, left, left right left") = secure {
    import bedazzle.either._
    val x: Either[Int, String] = 1.left
    val y: Either[String, Int] = 1.right

    x.swap == y
  }

  property("kestrel aka unsafeTap") = secure {
    import bedazzle.birds._
    
    var called = false
    val call: String => Unit = Function.const(called = true)
    val a = "xyzzy"
    val b = a <| call

    a == b && called
  }

  property("thrush") = secure {
    import bedazzle.all._

    val a = 1
    val f: Int => String = _.toString
    val b = a |> f

    b == "1"
  }
}

