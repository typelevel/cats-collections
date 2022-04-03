/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.collections

import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class DiscreteSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  property("succ")(forAll { (x: Int) =>
    assertEquals(Discrete[Int].succ(x), (x + 1))
  })

  property("pred")(forAll { (x: Int) =>
    assertEquals(Discrete[Int].pred(x), (x - 1))
  })

  property("adj")(forAll { (x: Int) =>
    assert(Discrete[Int].adj(x, x + 1))
  })

  property("non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x + 1 == y) y + 2 else y
    assertEquals(Discrete[Int].adj(x, yy), false)
  })

  property("inverse succ")(forAll { (x: Int) =>
    assertEquals(Discrete[Int].inverse.succ(x), (x - 1))
  })

  property("inverse pred")(forAll { (x: Int) =>
    assertEquals(Discrete[Int].inverse.pred(x), (x + 1))
  })

  property("inverse adj")(forAll { (x: Int) =>
    assert(Discrete[Int].inverse.adj(x, x - 1))
  })

  property("inverse non adj")(forAll { (x: Int, y: Int) =>
    val yy = if (x - 1 == y) y - 2 else y
    assertEquals(Discrete[Int].inverse.adj(x, yy), false)
  })
}
