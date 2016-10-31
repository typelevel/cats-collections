package dogs
package tests

import Predef._

class OptionSpec extends DogsSuite {
  import Option._

  test("none does not contain anything") {
    forAll { (x: Int) =>
      none[Int].contains(x) shouldBe false
    }
  }

  test("some(x) contains itself") {
    forAll { (x: Int) =>
      some(x).contains(x) shouldBe true
      some(x).contains(x + 1) shouldBe false
    }
  }

  test("nothing to find in none") {
    forAll { (x: String) =>
      none[String].find(_.length >= 0) shouldBe none[String]
    }
  }

  test("find in some(x)") {
    forAll { (x: String) =>
      some(x).find(_.length >= 0) shouldBe some(x)
      some(x).find(_.length < 0) shouldBe none[String]
    }
  }

  test("lastOption for none") {
    forAll { (x: Int) =>
      none[Int].lastOption shouldBe none[Int]
    }
  }

  test("lastOption in some(x)") {
    forAll { (x: Int) =>
      some(x).lastOption shouldBe some(x)
    }
  }

  test("headOption for none") {
    forAll { (x: Int) =>
      none[Int].headOption shouldBe none[Int]
    }
  }

  test("headOption in some(x)") {
    forAll { (x: Int) =>
      some(x).headOption shouldBe some(x)
    }
  }

  test("collect"){
    forAll { (x: Int) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
      some(x).collect(pf).toScalaOption shouldBe scala.Some(x).collect(pf)
      none[Int].collect(pf).toScalaOption shouldBe scala.None
    }
  }

  test("collectFirst"){
    forAll { (x: Int) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
      some(x).collectFirst(pf).toScalaOption shouldBe scala.Some(x).collectFirst(pf)
      none[Int].collectFirst(pf).toScalaOption shouldBe scala.None
    }
  }

}
