package dogs
package tests

import cats.implicits._
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

}
