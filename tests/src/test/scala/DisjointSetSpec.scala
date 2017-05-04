package dogs
package tests

import Predef._
import dogs.tests.arbitrary._
import cats.implicits._



class DisjointSetSpec extends DogsSuite with ArbitrarySet with ArbitraryList {

  /* Core tests, test component size pre/post union of edges. use any sized sets. */

  test("initial number of components equals size of the set")(forAll { (xs: Set[Int]) =>
    DisjointSet(xs).components should equal(xs.size)
  })

  test("union of all elements should yield a single component")(forAll { (xs: Set[Int]) => {
    val edges = xs.toScalaSet.iterator.sliding(2).toList
    val set = edges.foldLeft(DisjointSet(xs))((acc, v) => acc.union(v.head, v.last).getOrElse(acc))
    if (!xs.isEmpty) set.components should equal(1) else set.components should equal(0)
  }})

  test("find of an element not in the set should return None")(forAll { (xs: Set[Int], ys: Set[Int]) => {
    val d = xs.diff(ys).toList()
    val set = DisjointSet(ys)
    if (!d.isEmpty) d.forall(set.find(_).isNone)
  }})

  test("find of an element in the set should be defined")(forAll { (xs: Set[Int]) => {
    val set = DisjointSet(xs)
    xs.toList().forall(set.find(_).isDefined)
  }})


  /* Check empty sets, etc. */
  test("number of components of an empty set is zero") {
    DisjointSet(Set.empty[Int]).components should equal(0)
  }

  test("union of element outside of set yields None()") {
    DisjointSet(Set(1,2)).union(2,3) should be(None())
    DisjointSet(Set(2,3)).union(1,2) should be(None())
    DisjointSet(Set(1,2)).union(3,4) should be(None())
  }

  test("incremental union of elements reduces number of components by 1") {
    val set = DisjointSet(Set(1,2,3,4,5)).union(1,5)
    set.flatMap[Int](x => Some(x.components)) should be (Some(4))
  }


}
