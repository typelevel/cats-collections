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

import cats.Eval
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Prop._

class AnotherDietSuite extends DisciplineSuite {
  import DietSuite._

  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default
      .withMinSuccessfulTests(if (BuildInfo.isJvm) 300 else 30)

  property("foldLeft")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    assertEquals(rs.toDiet.foldLeft(start)(f), rs.toSet.toList.sorted.foldLeft(start)(f))
  })

  property("foldLeft/toList")(forAll { (rs: Ranges) =>
    assertEquals(rs.toDiet.foldLeft(List.empty[Int])(_ :+ _), rs.toDiet.toList)
  })

  property("foldRight")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    assertEquals(rs.toDiet.foldRight(Eval.now(start))((v, acc) => acc.map(f(v, _))).value,
                 rs.toSet.toList.sorted.foldRight(start)(f)
    )
  })

  property("foldRight/toList")(forAll { (rs: Ranges) =>
    assertEquals(
      rs.toDiet.foldRight(Eval.now(List.empty[Int]))((v, acc) => acc.map(v :: _)).value,
      rs.toDiet.toList
    )
  })

  property("not be modified when inserting existing item")(forAll { (d: Diet[Int]) =>
    d.toList.forall(elem =>
      // there may be structural changes, so fall back to list comparison
      d.add(elem).toList == d.toList
    )
  })

  property("--")(forAll { (d1: Diet[Int], d2: Diet[Int]) =>
    val d = d1 -- d2
    d2.toList.foreach(elem => assert(!d.contains(elem)))
    d1.toList.foreach(elem => assert(d2.contains(elem) || d.contains(elem)))
  })
}
