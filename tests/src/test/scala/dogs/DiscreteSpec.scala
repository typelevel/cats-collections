/**
 * Created by nperez on 2/4/16.
 */


package dogs
package tests

import org.scalatest.{Matchers, FlatSpec}


class DietTest extends FlatSpec with Matchers {

  import dogs.Predef._
  import Ext._

  implicit val d = new BigIntDiscrete()

//  "diet" should "return node with value range when inserting into empty" in {
//
//    val diet = Diet[BigInt]
//
//    val result = diet.add(5)
//
//    result.min should be (Some(5))
//    result.max should be (Some(5))
//  }

  it should "create a new node when add not adj item" in {
    val diet = Diet[BigInt].add(5).add(3).add(7)

    val result = diet.disjoins()(d).map(l => l.toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(3), scala.List(5), scala.List(7))
  }

  it should "join nodes when item adj to existing seq" in {
    val diet = Diet[BigInt].add(5).add(6).add(1).add(3).add(2).add(8)

    val result = diet.disjoins()(d).map(l => l.toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(1, 2, 3), scala.List(5, 6), scala.List(8))
  }
}

