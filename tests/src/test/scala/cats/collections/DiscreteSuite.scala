package cats.collections

import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class DiscreteSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  property("succ")(forAll { x: Int =>
    assertEquals(Discrete[Int].succ(x), (x + 1))
  })

  property("pred")(forAll { x: Int =>
    assertEquals(Discrete[Int].pred(x), (x - 1))
  })

  property("adj")(forAll { x: Int =>
    assert(Discrete[Int].adj(x, x + 1))
  })

  property("non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x + 1 == y) y + 2 else y
    assertEquals(Discrete[Int].adj(x, yy), false)
  })

  property("inverse succ")(forAll { x: Int =>
    assertEquals(Discrete[Int].inverse.succ(x), (x - 1))
  })

  property("inverse pred")(forAll { x: Int =>
    assertEquals(Discrete[Int].inverse.pred(x), (x + 1))
  })

  property("inverse adj")(forAll { x: Int =>
    assert(Discrete[Int].inverse.adj(x, x - 1))
  })

  property("inverse non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x - 1 == y) y - 2 else y
    assertEquals(Discrete[Int].inverse.adj(x, yy), false)
  })
}
