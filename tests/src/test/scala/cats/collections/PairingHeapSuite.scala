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

import cats.{Order, Show}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen, Gen, Properties, Test}

object PairingHeapSuite {
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
}

class PairingHeapSuite extends Properties("PairingHeap") {
  import PairingHeapSuite._

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p
      .withMinSuccessfulTests(10000)
      .withMaxDiscardRatio(if (BuildInfo.isJvm) 5 else 50)
      .withMinSize(0)
      .withWorkers(if (BuildInfo.isJvm) 2 else 1)

  property("sorted") = forAll { (list: List[Int]) =>

    val heap = list.foldLeft(PairingHeap.empty[Int])((h, i) => h.add(i))

    heap.toList == list.sorted
  }

  property("fromIterable is sorted") = forAll { (list: List[Int]) =>
    val heap = PairingHeap.fromIterable(list)
    val heapList = heap.toList
    val heap1 = PairingHeap.fromIterable(heapList)

    (heapList == list.sorted) && Order[PairingHeap[Int]].eqv(heap, heap1)
  }

  property("size is consistent with isEmpty/nonEmpty") = forAll { (heap: PairingHeap[Int]) =>
    val a = heap.isEmpty == (heap.size == 0)
    val b = heap.nonEmpty == (heap.size > 0)
    val c = heap.isEmpty == (!heap.nonEmpty)

    a && b && c
  }

  property("fromIterable is the same as adding") = forAll { (init: List[Int]) =>
    val heap1 = PairingHeap.fromIterable(init)
    val heap2 = init.foldLeft(PairingHeap.empty[Int])(_.add(_))

    heap1.toList == heap2.toList
  }

  property("minimumOption after removing one is >= before") = forAll { (heap: PairingHeap[Int]) =>
    val min0 = heap.minimumOption
    val min1 = heap.remove.minimumOption

    (min0, min1) match {
      case (None, next) => next.isEmpty
      case (_, None)    => heap.size == 1
      case (Some(m0), Some(m1)) =>
        m0 <= m1
    }
  }

  property("PairingHeap.minimumOption is the real minimum") = {
    def heapLaw(heap: PairingHeap[Int]): Boolean =
      heap.minimumOption match {
        case None => heap.isEmpty
        case Some(min) =>
          val heap1 = heap.remove
          heap1.isEmpty || {
            min <= heap1.toList.min
          }
      }

    forAll { (heap: PairingHeap[Int]) =>
      val a = heapLaw(heap)
      // even after removing this is true
      val b = heapLaw(heap.remove)
      val c = heapLaw(heap.remove.remove)
      val d = heapLaw(heap.remove.remove.remove)

      a && b && c && d && PairingHeap.empty[Int].minimumOption.isEmpty
    }
  }

  property("PairingHeap.foldLeft is consistent with toList.foldLeft") = forAll {
    (heap: PairingHeap[Int], init: Long, fn: (Long, Int) => Long) =>
      heap.foldLeft(init)(fn) == heap.toList.foldLeft(init)(fn)
  }

  property("Show[PairingHeap[Int]] works like toList.mkString") = forAll { (heap: PairingHeap[Int]) =>
    Show[PairingHeap[Int]].show(heap) == heap.toList.mkString("PairingHeap(", ", ", ")")
  }

  property("Order[PairingHeap[Int]] works like List[Int]") = forAll { (a: PairingHeap[Int], b: PairingHeap[Int]) =>
    Order[PairingHeap[Int]].compare(a, b) == Order[List[Int]].compare(a.toList, b.toList)
  }

  property("PairingHeap.exists is correct") = forAll { (a: PairingHeap[Int], fn: Int => Boolean) =>
    a.exists(fn) == a.toList.exists(fn)
  }

  property("PairingHeap.forall is correct") = forAll { (a: PairingHeap[Int], fn: Int => Boolean) =>
    a.forall(fn) == a.toList.forall(fn)
  }

  property("PairingHeap.empty is less than nonEmpty") = forAll { (item: Int, heap: PairingHeap[Int]) =>
    val ord = Order[PairingHeap[Int]]
    val a = ord.lteqv(PairingHeap.empty, heap)
    val b = ord.lt(PairingHeap.empty, PairingHeap.empty + item)
    val c = ord.gteqv(heap, PairingHeap.empty)
    val d = ord.gt(PairingHeap.empty + item, PairingHeap.empty)

    a && b && c && d
  }

  def genPairingHeapLimitedSize(limit: Int): Gen[PairingHeap[Int]] =
    Gen.choose(0, limit).flatMap(heapGen[Int](_, Arbitrary.arbitrary[Int]))

  property("PairingHeap.combineAll (remove) does < log_2 N + 8 work") = forAll(genPairingHeapLimitedSize(10)) {
    (heap: PairingHeap[Int]) =>
      def isGood(heap: PairingHeap[Int]): Boolean = {
        if (heap.size > heap.subtrees.size) {

          val totalSize = heap.size
          val logValue = math.ceil(math.log(totalSize.toDouble) / math.log(2)).toInt

          val bound = if (totalSize > 0) (logValue + 8.0).toInt else 1
          val check = heap.subtrees.size <= bound

          check && heap.subtrees.forall(isGood(_))
        } else true
      }

      isGood(heap)
  }

  property("PairingHeap satisfies the heap property") = {
    def isHeap[A: Order](h: PairingHeap[A]): Boolean =
      h.minimumOption match {
        case None =>
          h.subtrees.isEmpty && h.isEmpty

        case Some(m) =>
          h.subtrees.forall { sh =>
            sh.minimumOption.forall { sm => Order[A].lteqv(m, sm) } && isHeap(sh)
          }
      }

    forAll { (h: PairingHeap[Int]) => isHeap(h) }
  }

  property("takeLargest is the same as sort.reverse.take") = forAll { (as: Iterable[Int], k: Int) =>
    PairingHeap.takeLargest(as, k).toList.reverse == as.toList.sorted.reverse.take(k)
  }

  property("pop and remove return the same heap") = forAll { (heap: PairingHeap[Int]) =>
    val heap1 = heap.remove
    heap.pop.map(_._2) match {
      case Some(heap2) => Order[PairingHeap[Int]].eqv(heap1, heap2)
      case None        => heap1.isEmpty
    }
  }

  property("pop returns the minimum element") = forAll { (heap: PairingHeap[Int]) =>
    val min1 = heap.pop.map(_._1)
    val min2 = heap.minimumOption
    min1 == min2
  }
}
