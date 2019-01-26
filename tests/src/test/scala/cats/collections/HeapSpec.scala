package cats.collections
package tests

import cats.{Order, Show}
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

/**
 * Created by nperez on 3/28/16.
 */
class HeapSpec extends CatsSuite {
  implicit val propConfig =
    PropertyCheckConfig(minSuccessful = 200)

  test("sorted")(
    forAll { (set: Set[Int]) =>

      val heap = set.foldLeft(Heap.empty[Int])((h, i) => h.add(i))

      val exp = set.toList

      heap.toList should be(exp.sorted)

    })

  test("heapify is sorted") {
    forAll { (set: Set[Int]) =>
      val setList = set.toList
      val heap = Heap.empty[Int].heapify(setList)

      assert(heap.toList == setList.sorted)
    }
  }

  def heapGen[A: Order](size: Int, agen: Gen[A]): Gen[Heap[A]] = {
    val listA = Gen.listOfN(size, agen)
    val startWith1 =
      listA.map {
        case Nil => Heap.empty[A]
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
      }
      else Gen.const(Heap.empty[A])

    Gen.frequency((2, addOnly), (3, startWith1), (5, heapify), (1, addMoreAndRemove), (1, smallerAdd))
  }

  implicit def arbHeap[A: Arbitrary: Order]: Arbitrary[Heap[A]] =
    Arbitrary {
      Gen.choose(0, 10000).flatMap(heapGen[A](_, Arbitrary.arbitrary[A]))
    }

  test("adding increases size") {
    forAll { (heap: Heap[Int], x: Int) =>
      val heap1 = heap + x
      assert(heap1.size == (heap.size + 1))
    }
  }

  test("remove decreases size") {
    forAll { (heap: Heap[Int]) =>
      val heap1 = heap.remove
      assert((heap1.size == (heap.size - 1)) || (heap1.isEmpty && heap.isEmpty))
    }

    assert(Heap.empty[Int].remove == Heap.empty[Int])
  }

  test("size is consistent with isEmpty") {
    forAll { (heap: Heap[Int]) =>
      assert(heap.isEmpty == (heap.size == 0))
    }
  }

  test("height is O(log N) for all heaps") {
    forAll { (heap: Heap[Int]) =>
      val bound = math.log(heap.size.toDouble) / math.log(2.0) + 1.0
      assert(heap.isEmpty || heap.height.toDouble <= bound)
    }
  }

  test("heapify is the same as adding") {
    forAll { (init: List[Int]) =>
      val heap1 = Heap.fromIterable(init)
      val heap2 = init.foldLeft(Heap.empty[Int])(_.add(_))
      assert(heap1.toList == heap2.toList)
    }
  }

  test("getMin after removing one is >= before") {
    forAll { (heap: Heap[Int]) =>
      val min0 = heap.getMin
      val min1 = heap.remove.getMin

      (min0, min1) match {
        case (None, next) => assert(next.isEmpty)
        case (_, None) => assert(heap.size == 1)
        case (Some(m0), Some(m1)) =>
          assert(m0 <= m1)
      }
    }
  }

  test("Heap.getMin is the real minimum") {
    def heapLaw(heap: Heap[Int]) =
      heap.getMin match {
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

    assert(Heap.empty[Int].getMin.isEmpty)
  }

  test("Heap.foldLeft is consistent with toList.foldLeft") {
    forAll { (heap: Heap[Int], init: Long, fn: (Long, Int) => Long) =>
      assert(heap.foldLeft(init)(fn) == heap.toList.foldLeft(init)(fn))
    }
  }

  test("Show[Heap[Int]] works like toList.mkString") {
    forAll { (heap: Heap[Int]) =>
      assert(Show[Heap[Int]].show(heap) == heap.toList.mkString("Heap(", ", ", ")"))
    }
  }
}
