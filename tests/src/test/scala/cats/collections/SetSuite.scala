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

import cats.collections.arbitrary.set._
import cats.kernel.Eq
import cats.Show
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class SetSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  test("set is always sorted")(forAll { (xs: List[Int]) =>
    val tree = AvlSet.fromList(xs)

    val ours: List[Int] = tree.toList
    val theirs: List[Int] = xs.distinct.sorted

    assertEquals(ours, theirs)
  })

  property("iterator works")(forAll { (xs: AvlSet[Int]) =>
    assertEquals(xs.toIterator.toList, xs.toList)
  })

  property("equality")(forAll { (xs: List[Int]) =>
    val t1 = AvlSet.fromList(xs)
    val t2 = AvlSet.fromList(xs.reverse)
    if (t1 != t2) {
      assert(Eq[AvlSet[Int]].eqv(t1, t2))
    }
  })

  property("inequality")(forAll { (xs: List[Int], ys: List[Int]) =>
    val t1 = AvlSet.fromList(xs)
    val t2 = AvlSet.fromList(ys)
    if (Set(xs: _*) != Set(ys: _*)) {
      assertEquals(Eq[AvlSet[Int]].eqv(t1, t2), false)
    }
  })

  import AvlSet._
  def balanced[A](t: AvlSet[A]): Boolean = t match {
    case BTNil           => true
    case Branch(_, l, r) =>
      java.lang.Math.abs(l.height - r.height) <= 1 && balanced(l) && balanced(r)
  }

  property("set is always balanced")(forAll { (xs: List[Int]) =>
    val tree = AvlSet(xs: _*)
    assert(balanced(tree))
  })

  property("set can delete")(forAll { (xs: Map[Int, Boolean]) =>
    val tree = AvlSet(xs.keySet.toSeq: _*)
    val filtered = xs.foldLeft(tree)((t, i) => if (i._2) t else t.remove(i._1))

    val ours: Set[Int] = filtered.to[Set]
    val theirs: Set[Int] = xs.collect { case (i, true) => i }.toSet

    assertEquals(ours, theirs)
    assert(balanced(filtered))
  })

  property("contains works")(forAll { (xs: Map[Int, Boolean]) =>
    val tree = xs.foldLeft[AvlSet[Int]](AvlSet.empty)((t, i) => if (i._2) t + i._1 else t)

    assert(
      xs.forall { case (k, v) =>
        tree.contains(k) == v
      }
    )
  })

  property("find works")(forAll { (xs: Map[Int, Boolean]) =>
    val tree = xs.foldLeft[AvlSet[Int]](AvlSet.empty)((t, i) => if (i._2) t + i._1 else t)

    assert(
      xs.forall { case (k, v) =>
        tree.find(_ == k).isDefined == v
      }
    )
  })

  property("intersect is correct")(forAll { (xs: Set[Int], ys: Set[Int]) =>
    val xt = AvlSet(xs.toSeq: _*)
    val yt = AvlSet(ys.toSeq: _*)

    assertEquals(xt.intersect(yt).toScalaSet, xs.intersect(ys))
    assertEquals((xt & yt).toScalaSet, xs.intersect(ys))
  })

  property("union is correct")(forAll { (xs: Set[Int], ys: Set[Int]) =>
    val xt = AvlSet(xs.toSeq: _*)
    val yt = AvlSet(ys.toSeq: _*)

    assertEquals(xt.union(yt).toScalaSet, xs.union(ys))
    assertEquals((xt | yt).toScalaSet, xs.union(ys))
  })

  property("we can take the difference of sets")(forAll { (xs: Set[Int], ys: Set[Int]) =>
    val xt = AvlSet(xs.toSeq: _*)
    val yt = AvlSet(ys.toSeq: _*)

    assertEquals(xt.diff(yt).toScalaSet, xs.diff(ys))
    assertEquals((xt - yt).toScalaSet, xs.diff(ys))
  })

  property("map works")(forAll { (xs: Set[Int]) =>
    val f: Int => Int = _ + 1

    val xt = AvlSet(xs.toSeq: _*)

    assertEquals(xt.map(f).toScalaSet, xs.map(f))
  })

  property("fromFoldable works")(forAll { (xs: List[Int]) =>
    val xt = AvlSet.fromFoldable(xs)

    assertEquals(xt.toScalaSet, xs.toSet)
  })

  property("Show instance is consistent with toString")(forAll { (as: AvlSet[Int]) =>
    assertEquals(as.toString, Show[AvlSet[Int]].show(as))
  })
}
