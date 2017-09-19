package dogs
package tests

import cats.implicits._

/**
 * Created by nperez on 3/28/16.
 */
class HeapSpec extends DogsSuite {
  test("sorted")(
    forAll { (xs: scala.List[Int]) =>

      val set = xs.toSet

      val heap = set.foldLeft(Heap.empty[Int])((h, i) => h.add(i))

      val exp = set.toList

      heap.toList should be(exp.sorted)

    })
}
