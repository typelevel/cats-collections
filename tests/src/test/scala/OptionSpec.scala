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

}
