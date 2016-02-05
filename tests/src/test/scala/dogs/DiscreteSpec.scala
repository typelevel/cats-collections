/**
 * Created by Nicolas A Perez (@anicolaspp) on 2/4/16.
 */


package dogs
package tests

import dogs.Diet.EmptyDiet
import dogs.Order.{EQ, GT, LT, Ordering}
import dogs.Predef._
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}
import org.scalatest.{FlatSpec, Matchers}

class BigIntDiscrete extends Discrete[BigInt] {
  override def succ(x: BigInt): BigInt = x + 1

  override def pred(x: BigInt): BigInt = x - 1

  override def apply(l: BigInt, r: BigInt): Ordering = if (l < r) LT else if (l > r) GT else EQ
}
object Ext {
  implicit def toBigIntDiscrete(x: BigInt) = new BigIntDiscrete()
}

class DietTest extends FlatSpec with Matchers {

  import dogs.Predef._

  implicit val d = new BigIntDiscrete()

  "diet" should "return node with value range when inserting into empty" in {

    val diet = Diet[BigInt]

    val result = diet.add(5)

    result.min should be (Some(5))
    result.max should be (Some(5))
  }

  it should "have min and max" in {
    val diet = Diet[BigInt].add(5).add(3).add(7)

    diet.min should be(Some(3))
    diet.max should be(Some(7))
  }

  it should "create a new node when add not adj item" in {
    val diet = Diet[BigInt].add(5).add(3).add(7)

    val result = diet.disjointSets().map(l => l.generate().toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(3), scala.List(5), scala.List(7))
  }

  it should "join nodes when item adj to existing seq" in {
    val diet = Diet[BigInt].add(5).add(6).add(1).add(3).add(2).add(8)

    val result = diet.disjointSets().map(l => l.generate().toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(1, 2, 3), scala.List(5, 6), scala.List(8))
  }

  it should "be always sorted" in {
    val diet = Diet[BigInt].add(5).add(6).add(1).add(3).add(2).add(8)

    val sorted = diet.toList().toScalaList

    sorted should contain inOrderOnly (1, 2, 3, 5, 6, 8)
  }

  it should "add disjoint range" in {
    val diet = Diet[BigInt]

    val result = diet.add(0, 100)

    val other = result.disjointSets().map(l => l.generate().toScalaList).toScalaList

    other should contain ((Range(0, 101)).toList)
  }

  it should "join disjoint range" in {
    val diet = Diet[BigInt] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList.toScalaList should contain inOrderOnly (1, 2, 3, 4, 5, 6, 7)
  }

  it should "contain items from range" in {
    val diet = Diet[BigInt].add(5, 10).add(1, 3).add(12, 20)

    diet.contains(1) should be (true)
    diet.contains(2) should be (true)
    diet.contains(3) should be (true)

    diet.contains(4) should be (false)

    diet.contains(6) should be (true)

    diet.contains(15) should be (true)
  }

  it should "return empty when removing from empty" in {
    Diet[BigInt].remove(1) should be (EmptyDiet())
  }

  it should "not be modified when removing non existed item" in {

    val diet = Diet[BigInt] + 1 +2 + 3 + 5

    diet.remove(4) should be (diet)
  }

  it should "be split when removing from range" in {
    val diet = Diet[BigInt] + 1 +2 + 3 + 5

    val other = diet.remove(2).disjointSets().map(x => x.generate().toScalaList).toScalaList

    other should contain  inOrder (scala.List(1), scala.List(3), scala.List(5))
  }

  it should "map" in {
    val diet = Diet[BigInt] + 1 +2 + 8 + 5

    val other = diet.map(x => x + 2).disjointSets().map(x => x.generate().toScalaList).toScalaList

    other should contain inOrderOnly(scala.List(3,4), scala.List(7), scala.List(10))
  }

  it should "foldLeft" in {
    val diet = Diet[BigInt] + 1 +2 + 8 + 5

    diet.foldLeft(BigInt(10))((a: BigInt,b: BigInt) => a + b) should be (26)
    diet.foldRight(BigInt(10))((a: BigInt,b: BigInt) => a + b) should be (26)
  }
}

