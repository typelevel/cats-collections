package cats.collections
package tests

import cats.tests.CatsSuite

class DisjointSetsSpec extends CatsSuite {

  test("union-find operations using state/stator monad") {

    import DisjointSets._

    val operations = for {
      _ <- union(1, 2)
      oneAndTwo <- find(2)
      _ <- union(3, 4)
      threeAndFour <- find(3)
      _ <- union(2, 3)
      allFromOne <- find(1)
      allFromTwo <- find(2)
      allFromThree <- find(3)
      allFromFour <- find(4)
    } yield (
      oneAndTwo,
      threeAndFour,
      allFromOne,
      allFromTwo,
      allFromThree,
      allFromFour
    )

    val (
      Some(a),
      Some(b),
      Some(c),
      Some(d),
      Some(e),
      Some(f)
    ) = operations.runA(DisjointSets(1, 2, 3, 4)).value

    (a should not).equal(b)
    c shouldBe d
    d shouldBe e
    e shouldBe f

  }

  test("build unions with disjoint sets as if a set of sets were used") {

    import scala.collection.immutable.Range

    val numbers = Range(0, 200)

    val classifiedNumbers = numbers.foldLeft(DisjointSets(numbers: _*)) { (dsets, v) =>
      dsets.union(v, v % 10)._1
    }

    val groupByClassification = numbers.groupBy(_ % 10).mapValues(_.toSet)
    val (_, disjointSetsClassification) = classifiedNumbers.toSets

    disjointSetsClassification.toScalaMap.mapValues(_.toScalaSet).toMap should be(groupByClassification.toMap)
  }

}
