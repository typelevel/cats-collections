package cats.collections
package tests

import cats.{Order, Show}
import cats.collections.laws.discipline.PartiallyOrderedSetTests
import cats.kernel.laws.discipline.{CommutativeMonoidTests, OrderTests}
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}

class PairingHeapSpec extends CatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration.copy(
      minSuccessful = 10000
    )

  def heapGen[A: Order](size: Int, agen: Gen[A]): Gen[PairingHeap[A]] = {
    val listA = Gen.listOfN(size, agen)
    val startWith1 =
      listA.map {
        case Nil       => PairingHeap.empty[A]
        case h :: tail => tail.foldLeft(PairingHeap(h))(_.add(_))
      }
    val addOnly = listA.map(_.foldLeft(PairingHeap.empty[A])(_.add(_)))
    val heapify = listA.map(PairingHeap.fromIterable(_))
    // This one is recursive and with small probability can get quite deep
    val addMoreAndRemove: Gen[PairingHeap[A]] =
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
      } else Gen.const(PairingHeap.empty[A])

    val merged =
      for {
        rightSize <- Gen.choose(0, size)
        leftSize = size - rightSize
        right <- heapGen(rightSize, agen)
        left <- heapGen(leftSize, agen)
      } yield right.combine(left)

    Gen.frequency((2, addOnly), (3, startWith1), (5, heapify), (1, addMoreAndRemove), (1, smallerAdd), (1, merged))
  }

  implicit def arbPairingHeap[A: Arbitrary: Order]: Arbitrary[PairingHeap[A]] =
    Arbitrary {
      Gen.sized(heapGen[A](_, Arbitrary.arbitrary[A]))
    }

  implicit def cogenPairingHeap[A: Cogen: Order]: Cogen[PairingHeap[A]] =
    Cogen[List[A]].contramap { (h: PairingHeap[A]) => h.toList }

  checkAll("PartiallyOrderedSet[PairingHeap]", PartiallyOrderedSetTests[PairingHeap].partiallyOrderedSet[Long, Int])

  checkAll("Order[PairingHeap[Int]]", OrderTests[PairingHeap[Int]].order)

  checkAll("PairingHeap[Int]", CommutativeMonoidTests[PairingHeap[Int]].commutativeMonoid)

  test("sorted")(forAll { (list: List[Int]) =>

    val heap = list.foldLeft(PairingHeap.empty[Int])((h, i) => h.add(i))

    heap.toList should be(list.sorted)

  })

  test("fromIterable is sorted") {
    forAll { (list: List[Int]) =>
      val heap = PairingHeap.fromIterable(list)
      val heapList = heap.toList
      val heap1 = PairingHeap.fromIterable(heapList)

      assert(heapList == list.sorted)
      assert(Order[PairingHeap[Int]].eqv(heap, heap1))
    }
  }

  test("size is consistent with isEmpty/nonEmpty") {
    forAll { (heap: PairingHeap[Int]) =>
      assert(heap.isEmpty == (heap.size == 0))
      assert(heap.nonEmpty == (heap.size > 0))
      assert(heap.isEmpty == (!heap.nonEmpty))
    }
  }

  test("fromIterable is the same as adding") {
    forAll { (init: List[Int]) =>
      val heap1 = PairingHeap.fromIterable(init)
      val heap2 = init.foldLeft(PairingHeap.empty[Int])(_.add(_))
      assert(heap1.toList == heap2.toList)
    }
  }

  test("minimumOption after removing one is >= before") {
    forAll { (heap: PairingHeap[Int]) =>
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

  test("PairingHeap.minimumOption is the real minimum") {
    def heapLaw(heap: PairingHeap[Int]) =
      heap.minimumOption match {
        case None => assert(heap.isEmpty)
        case Some(min) =>
          val heap1 = heap.remove
          assert(heap1.isEmpty || {
            min <= heap1.toList.min
          })
      }

    forAll { (heap: PairingHeap[Int]) =>
      heapLaw(heap)
      // even after removing this is true
      heapLaw(heap.remove)
      heapLaw(heap.remove.remove)
      heapLaw(heap.remove.remove.remove)
    }

    assert(PairingHeap.empty[Int].minimumOption.isEmpty)
  }

  test("PairingHeap.foldLeft is consistent with toList.foldLeft") {
    forAll { (heap: PairingHeap[Int], init: Long, fn: (Long, Int) => Long) =>
      assert(heap.foldLeft(init)(fn) == heap.toList.foldLeft(init)(fn))
    }
  }

  test("Show[PairingHeap[Int]] works like toList.mkString") {
    forAll { (heap: PairingHeap[Int]) =>
      assert(Show[PairingHeap[Int]].show(heap) == heap.toList.mkString("PairingHeap(", ", ", ")"))
    }
  }

  test("Order[PairingHeap[Int]] works like List[Int]") {
    forAll { (a: PairingHeap[Int], b: PairingHeap[Int]) =>
      assert(Order[PairingHeap[Int]].compare(a, b) == Order[List[Int]].compare(a.toList, b.toList))
    }
  }

  test("PairingHeap.exists is correct") {
    forAll { (a: PairingHeap[Int], fn: Int => Boolean) =>
      assert(a.exists(fn) == a.toList.exists(fn))
    }
  }

  test("PairingHeap.forall is correct") {
    forAll { (a: PairingHeap[Int], fn: Int => Boolean) =>
      assert(a.forall(fn) == a.toList.forall(fn))
    }
  }

  test("PairingHeap.empty is less than nonEmpty") {
    forAll { (item: Int, heap: PairingHeap[Int]) =>
      val ord = Order[PairingHeap[Int]]
      assert(ord.lteqv(PairingHeap.empty, heap))
      assert(ord.lt(PairingHeap.empty, PairingHeap.empty + item))
      assert(ord.gteqv(heap, PairingHeap.empty))
      assert(ord.gt(PairingHeap.empty + item, PairingHeap.empty))
    }
  }

  test("PairingHeap.combineAll (remove) does < log_2 N + 8 work") {
    forAll { (heap: PairingHeap[Int]) =>
      def isGood(heap: PairingHeap[Int]): Unit = {
        val totalSize = heap.size

        val bound = if (totalSize > 0) (math.log(totalSize.toDouble) + 8.0).toInt else 1
        assert(heap.subtrees.size <= bound)
        heap.subtrees.foreach(isGood(_))
      }

      isGood(heap)
    }
  }

  test("PairingHeap satisfies the heap property") {
    def isHeap[A: Order](h: PairingHeap[A]): Unit =
      h.minimumOption match {
        case None =>
          assert(h.subtrees.isEmpty)
          assert(h.isEmpty)
          ()
        case Some(m) =>
          h.subtrees.foreach { sh =>
            sh.minimumOption.foreach { sm => assert(Order[A].lteqv(m, sm)) }
            isHeap(sh)
          }
      }

    forAll { (h: PairingHeap[Int]) => isHeap(h) }
  }

  test("takeLargest is the same as sort.reverse.take") {
    forAll { (as: Iterable[Int], k: Int) =>
      assert(PairingHeap.takeLargest(as, k).toList.reverse == as.toList.sorted.reverse.take(k))
    }
  }

  test("pop and remove return the same heap") {
    forAll { (heap: PairingHeap[Int]) =>
      val heap1 = heap.remove
      heap.pop.map(_._2) match {
        case Some(heap2) => assert(Order[PairingHeap[Int]].eqv(heap1, heap2))
        case None        => assert(heap1.isEmpty)
      }
    }
  }

  test("pop returns the minimum element") {
    forAll { (heap: PairingHeap[Int]) =>
      val min1 = heap.pop.map(_._1)
      val min2 = heap.minimumOption
      assert(min1 == min2)
    }
  }
}
