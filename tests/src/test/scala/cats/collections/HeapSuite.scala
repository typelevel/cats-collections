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

import cats.collections.laws.discipline.PartiallyOrderedSetTests
import cats.kernel.laws.discipline.OrderTests
import cats.{Order, Show}
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen, Gen, Test}

class HeapSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default
      .withMinSuccessfulTests(1000)

  def heapGen[A: Order](size: Int, agen: Gen[A]): Gen[Heap[A]] = {
    val listA = Gen.listOfN(size, agen)
    val startWith1 =
      listA.map {
        case Nil       => Heap.empty[A]
        case h :: tail => tail.foldLeft(Heap(h))(_.add(_))
      }
    val addOnly = listA.map(_.foldLeft(Heap.empty[A])(_.add(_)))
    val heapify = listA.map(Heap.fromIterable(_))
    // This one is recursive and with small probability can get quite deep
    val addMoreAndRemove: Gen[Heap[A]] =
      for {
        extraSize <- Gen.choose(1, size + 1)
        withExtra <- Gen.lzy(heapGen[A](size + extraSize, agen))
      } yield (0 until extraSize).foldLeft(withExtra) { (h, _) => h.remove }
    // we can also make smaller one and add to it:
    val smallerAdd =
      if (size > 0) {
        for {
          a <- agen
          heap <- heapGen(size - 1, agen)
        } yield heap + a
      } else Gen.const(Heap.empty[A])

    Gen.frequency((2, addOnly), (3, startWith1), (5, heapify), (1, addMoreAndRemove), (1, smallerAdd))
  }

  implicit def arbHeap[A: Arbitrary: Order]: Arbitrary[Heap[A]] =
    Arbitrary {
      Gen.sized(heapGen[A](_, Arbitrary.arbitrary[A]))
    }

  implicit def cogenHeap[A: Cogen: Order]: Cogen[Heap[A]] =
    Cogen[List[A]].contramap { (h: Heap[A]) => h.toList }

  checkAll("PartiallyOrderedSet[Heap]", PartiallyOrderedSetTests[Heap].partiallyOrderedSet[Long, Int])

  checkAll("Order[Heap[Int]]", OrderTests[Heap[Int]].order)

  property("sorted")(forAll { (list: List[Int]) =>

    val heap = list.foldLeft(Heap.empty[Int])((h, i) => h.add(i))

    assertEquals(heap.toList, list.sorted)
  })

  property("heapify is sorted") {
    forAll { (list: List[Int]) =>
      val heap = Heap.heapify(list)
      val heapList = heap.toList
      val heap1 = Heap.heapify(heapList)

      assert(heapList == list.sorted)
      assert(Order[Heap[Int]].eqv(heap, heap1))
    }
  }

  property("adding increases size") {
    forAll { (heap: Heap[Int], x: Int) =>
      val heap1 = heap + x
      assert(heap1.size == (heap.size + 1))
    }
  }

  property("add is the same as +") {
    forAll { (heap: Heap[Int], x: Int) =>
      val heap1 = heap + x
      val heap2 = heap.add(x)
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  property("addAll is the same as ++") {
    forAll { (heap: Heap[Int], x: List[Int]) =>
      val heap1 = heap ++ x
      val heap2 = heap.addAll(x)
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  property("addAll is the same as folding with add") {
    forAll { (heap: Heap[Int], x: List[Int]) =>
      val heap1 = heap.addAll(x)
      val heap2 = x.foldLeft(heap)(_.add(_))
      assert(Order[Heap[Int]].eqv(heap1, heap2))
    }
  }

  property("remove decreases size") {
    forAll { (heap: Heap[Int]) =>
      val heap1 = heap.remove
      assert((heap1.size == (heap.size - 1)) || (heap1.isEmpty && heap.isEmpty))
    } && {
      val r = assert(Heap.empty[Int].remove == Heap.empty[Int])

      if (r == ()) passed else falsified
    }
  }

  property("pop and remove return the same heap") {
    forAll { (heap: Heap[Int]) =>
      val heap1 = heap.remove
      heap.pop.map(_._2) match {
        case Some(heap2) => assert(Order[Heap[Int]].eqv(heap1, heap2))
        case None        => assert(heap1.isEmpty)
      }
    }
  }

  property("pop returns the minimum element") {
    forAll { (heap: Heap[Int]) =>
      val min1 = heap.pop.map(_._1)
      val min2 = heap.minimumOption
      assert(min1 == min2)
    }
  }

  property("size is consistent with isEmpty/nonEmpty") {
    forAll { (heap: Heap[Int]) =>
      assert(heap.isEmpty == (heap.size == 0))
      assert(heap.nonEmpty == (heap.size > 0))
      assert(heap.isEmpty == (!heap.nonEmpty))
    }
  }

  property("height is <= log_2 N + 1 for all heaps") {
    forAll { (heap: Heap[Int]) =>
      val bound = math.log(heap.size.toDouble) / math.log(2.0) + 1.0
      assert(heap.isEmpty || heap.height.toDouble <= bound)
    }
  }

  property("heapify is the same as adding") {
    forAll { (init: List[Int]) =>
      val heap1 = Heap.fromIterable(init)
      val heap2 = init.foldLeft(Heap.empty[Int])(_.add(_))
      assert(heap1.toList == heap2.toList)
    }
  }

  property("minimumOption after removing one is >= before") {
    forAll { (heap: Heap[Int]) =>
      val min0 = heap.minimumOption
      val min1 = heap.remove.minimumOption

      (min0, min1) match {
        case (None, next) => assert(next.isEmpty)
        case (_, None)    => assert(heap.size == 1)
        case (Some(m0), Some(m1)) =>
          assert(m0 <= m1)
      }
    }
  }

  test("Heap.minimumOption is the real minimum") {
    def heapLaw(heap: Heap[Int]): Unit =
      heap.minimumOption match {
        case None => assert(heap.isEmpty)
        case Some(min) =>
          val heap1 = heap.remove
          assert(heap1.isEmpty || {
            min <= heap1.toList.min
          })
      }

    forAll { (heap: Heap[Int]) =>
      heapLaw(heap)
      // even after removing this is true
      heapLaw(heap.remove)
      heapLaw(heap.remove.remove)
      heapLaw(heap.remove.remove.remove)
    }

    assert(Heap.empty[Int].minimumOption.isEmpty)
  }

  property("Heap.foldLeft is consistent with toList.foldLeft") {
    forAll { (heap: Heap[Int], init: Long, fn: (Long, Int) => Long) =>
      assert(heap.foldLeft(init)(fn) == heap.toList.foldLeft(init)(fn))
    }
  }

  property("Show[Heap[Int]] works like toList.mkString") {
    forAll { (heap: Heap[Int]) =>
      assert(Show[Heap[Int]].show(heap) == heap.toList.mkString("Heap(", ", ", ")"))
    }
  }

  property("Order[Heap[Int]] works like List[Int]") {
    forAll { (a: Heap[Int], b: Heap[Int]) =>
      assert(Order[Heap[Int]].compare(a, b) == Order[List[Int]].compare(a.toList, b.toList))
    }
  }

  property("Heap.exists is correct") {
    forAll { (a: Heap[Int], fn: Int => Boolean) =>
      assert(a.exists(fn) == a.toList.exists(fn))
    }
  }

  property("Heap.forall is correct") {
    forAll { (a: Heap[Int], fn: Int => Boolean) =>
      assert(a.forall(fn) == a.toList.forall(fn))
    }
  }

  property("Heap.empty is less than nonEmpty") {
    forAll { (item: Int, heap: Heap[Int]) =>
      val ord = Order[Heap[Int]]
      assert(ord.lteqv(Heap.empty, heap))
      assert(ord.lt(Heap.empty, Heap.empty + item))
      assert(ord.gteqv(heap, Heap.empty))
      assert(ord.gt(Heap.empty + item, Heap.empty))
    }
  }

  property("takeLargest is the same as sort.reverse.take") {
    forAll { (as: Iterable[Int], k: Int) =>
      assert(Heap.takeLargest(as, k).toList.reverse == as.toList.sorted.reverse.take(k))
    }
  }

  property("Heap.toPairingHeap.toList == Heap.toList") {
    forAll { (h: Heap[Int]) =>
      assert(h.toPairingHeap.toList == h.toList)
    }
  }

  property("Heap property is always maintained") {
    def law[A](h: Heap[A], outer: Heap[A])(implicit ord: Order[A]): Unit =
      h match {
        case Heap.Leaf() => ()
        case Heap.Branch(min, left, right) =>
          lazy val heapStr = if (outer != h) s"in $outer, subheap $h" else h.toString
          left.getMin.foreach { m => assert(ord.gteqv(m, min), s"$heapStr violates heap order property on left") }
          right.getMin.foreach { m => assert(ord.gteqv(m, min), s"$heapStr violates heap order property on right") }
          // we expect fully balanced heaps, but we put the size on the left first:
          assert(left.size >= right.size, s"$heapStr has left size = ${left.size} vs right size = ${right.size}")
          assert((left.height == right.height) || (left.height == right.height + 1),
                 s"$heapStr has unbalanced height: ${left.height} vs ${right.height}"
          )

          law(left, outer)
          law(right, outer)
      }

    forAll { (h: Heap[Int]) =>
      law(h, h)
    }
  }
}
