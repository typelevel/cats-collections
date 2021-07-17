package cats.collections

import munit.DisciplineSuite
import cats.collections.laws.discipline.PartiallyOrderedSetTests
import cats.kernel.laws.discipline.{CommutativeMonoidTests, OrderTests}

class PairingHeapDisciplineSuite extends DisciplineSuite {
  import PairingHeapSuite._

  checkAll("PartiallyOrderedSet[PairingHeap]",
    PartiallyOrderedSetTests[PairingHeap].partiallyOrderedSet[Long, Int])

  checkAll("Order[PairingHeap[Int]]", OrderTests[PairingHeap[Int]].order)

  checkAll("PairingHeap[Int]", CommutativeMonoidTests[PairingHeap[Int]].commutativeMonoid)
}
