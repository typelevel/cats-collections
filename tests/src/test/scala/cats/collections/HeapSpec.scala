package cats.collections
package tests

import cats.Order
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

/**
 * Created by nperez on 3/28/16.
 */
class HeapSpec extends CatsSuite {
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
    val heapify = listA.map(Heap.fromList(_))
    val addMoreAndRemove: Gen[Heap[A]] =
      for {
        extraSize <- Gen.choose(1, size + 1)
        withExtra <- Gen.lzy(heapGen[A](size + extraSize, agen))
      } yield (0 until extraSize).foldLeft(withExtra) { (h, _) => h.remove }

    Gen.frequency((1, addOnly), (1, startWith1), (2, heapify), (1, addMoreAndRemove))
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
      val bound = 2.0 * (math.log(heap.size.toDouble) / math.log(2.0) + 1)
      assert(heap.isEmpty || heap.height.toDouble <= bound)
    }
  }

  test("heapify is the same as adding") {
    forAll { (init: List[Int]) =>
      val heap1 = Heap.fromList(init)
      val heap2 = init.foldLeft(Heap.empty[Int])(_.add(_))
      assert(heap1.toList == heap2.toList)
    }
  }

  test("Heap.getMin is the real minimum") {
    forAll { (heap: Heap[Int]) =>
      heap.getMin match {
        case None => assert(heap.isEmpty)
        case Some(min) =>
          val heap1 = heap.remove
          assert(heap1.isEmpty || {
            min <= heap1.toList.min
          })
      }
    }

    assert(Heap.empty[Int].getMin.isEmpty)
  }
}
