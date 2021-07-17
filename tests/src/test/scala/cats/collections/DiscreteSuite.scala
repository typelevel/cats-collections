package cats.collections

import munit.DisciplineSuite
import org.scalacheck.Prop._

class DiscreteSuite extends DisciplineSuite {
  test("succ")(forAll { x: Int =>
    assertEquals(Discrete[Int].succ(x), (x + 1))
  })

  test("pred")(forAll { x: Int =>
    assertEquals(Discrete[Int].pred(x), (x - 1))
  })

  test("adj")(forAll { x: Int =>
    assert(Discrete[Int].adj(x, x + 1))
  })

  test("non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x + 1 == y) y + 2 else y
    assertEquals(Discrete[Int].adj(x, yy), false)
  })

  test("inverse succ")(forAll { x: Int =>
    assertEquals(Discrete[Int].inverse.succ(x), (x - 1))
  })

  test("inverse pred")(forAll { x: Int =>
    assertEquals(Discrete[Int].inverse.pred(x), (x + 1))
  })

  test("inverse adj")(forAll { x: Int =>
    assert(Discrete[Int].inverse.adj(x, x - 1))
  })

  test("inverse non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x - 1 == y) y - 2 else y
    assertEquals(Discrete[Int].inverse.adj(x, yy), false)
  })
}
