package dogs
package tests

import dogs.Predef._
import dogs.tests.arbitrary.all._
import org.scalatest.Matchers
import cats._
import cats.implicits._

import scala.collection.{SortedSet, Iterable}
import scala.collection.immutable.{List => SList}

/**
 * Created by nperez on 3/28/16.
 */
class HeapSpec extends DogsSuite {
  implicit class IterableOps[A](as: Iterable[A]) {
    def toScalaList: List[A] = List.fromIterable(as)
  }

  test("sorted")(
    forAll { (xs: scala.List[Int]) =>

      val set = xs.toSet

      var heap = set.foldLeft(Heap.empty[Int])((h, i) => h.add(i))


      val exp = dogs.List.fromIterable(set)

      heap.toList should matchToSorted(exp)

    })

  /// I need to verify the shape of the heap.
//  test("shape") {}
}
