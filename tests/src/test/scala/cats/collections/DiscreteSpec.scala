package cats.collections
package tests

import cats.tests.CatsSuite

class DiscreteSpec extends CatsSuite {

  test("succ")(forAll { (x: Int) =>
    Discrete[Int].succ(x) shouldEqual (x + 1)
  })

  test("pred")(forAll { (x: Int) =>
    Discrete[Int].pred(x) shouldEqual (x - 1)
  })

  test("adj")(forAll { (x: Int) =>
    Discrete[Int].adj(x, x + 1) shouldEqual true
  })

  test("non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x + 1 == y) y + 2 else y
    Discrete[Int].adj(x, yy) shouldEqual false
  })

  test("inverse succ")(forAll { (x: Int) =>
    Discrete[Int].inverse.succ(x) shouldEqual (x - 1)
  })

  test("inverse pred")(forAll { (x: Int) =>
    Discrete[Int].inverse.pred(x) shouldEqual (x + 1)
  })

  test("inverse adj")(forAll { (x: Int) =>
    Discrete[Int].inverse.adj(x, x - 1) shouldEqual true
  })

  test("inverse non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x - 1 == y) y - 2 else y
    Discrete[Int].inverse.adj(x, yy) shouldEqual false
  })
}
