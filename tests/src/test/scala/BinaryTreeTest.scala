/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs
package tests

import dogs.Order.Ordering
import dogs.Predef._
import dogs.std.intOrder
import dogs.tests.arbitrary._
import dogs.bedazzle.birds._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import org.scalatest._

object BinaryTreeSpec extends Properties("BinaryTree") with ArbitraryList {
  property("binary tree is always sorted") = forAll { (xs: List[Int]) =>
    val tree = xs.foldLeft(BinaryTree.empty[Int])(_ + _)

    val ours: scala.List[Int] = tree.toList.toScalaList
    val theirs: scala.List[Int] = xs.toScalaList.to[Set].to[Array].sorted.to[scala.List]

    ours == theirs
  }

  import BinaryTree._
  def balanced[A](t: BinaryTree[A]): Boolean = t match {
    case BTNil() => true
    case Branch(_, l, r) =>
      java.lang.Math.abs(l.height - r.height) <= 1 && balanced(l) && balanced(r)

  }

  property("binary tree is always balanced") = forAll { (xs: List[Int]) =>
    val tree = xs.foldLeft(BinaryTree.empty[Int])(_ + _)
    balanced(tree)
  }
}

