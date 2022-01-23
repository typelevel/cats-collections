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

import munit.FunSuite

class DisjointSetsSuite extends FunSuite {
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

    assertNotEquals(a, b)
    assertEquals(c, d)
    assertEquals(d, e)
    assertEquals(e, f)
  }

  test("build unions with disjoint sets as if a set of sets were used") {
    import scala.collection.immutable.Range

    val numbers = Range(0, 200)

    val classifiedNumbers = numbers.foldLeft(DisjointSets(numbers: _*)) { (dsets, v) =>
      dsets.union(v, v % 10)._1
    }

    val groupByClassification = numbers.groupBy(_ % 10).mapValues(_.toSet).toMap
    val (_, disjointSetsClassification) = classifiedNumbers.toSets

    assertEquals(
      disjointSetsClassification.toScalaMap.mapValues(_.toScalaSet).toMap,
      groupByClassification
    )
  }
}
