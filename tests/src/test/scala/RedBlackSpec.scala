package dogs
package tests

import org.scalacheck._
import org.scalacheck.Prop.forAll
import Arbitrary._
import cats._
import cats.std.all._
import cats.syntax.all._
import scala.{Boolean,Int,None,Option,Some}
import scala.collection.immutable.{List}
import scala.Predef._

object RedBlackSpec extends Properties("RedBlackTree") with ArbitraryLst {

  property("preorder traversal sees elements in ascending order") = forAll { xs: Lst[Int] =>
    val t = RedBlackTree.fromLst(xs)
    t.preorder[(Boolean,Option[Int])]((true, none), {
                 (b, a) => b match {
                   case (b, None) => (b, Some(a))
                   case (b, Some(aa)) => (b && Order[Int].lteqv(aa,a), Some(a))
                 }
               })._1


  }
  property("postorder traversal sees elements in descending order") = forAll { xs: Lst[Int] =>
    val t = RedBlackTree.fromLst(xs)
    t.postorder[(Boolean,Option[Int])]((true, none), {
                 (a, b) => b match {
                   case (b, None) => (b, Some(a))
                   case (b, Some(aa)) => (b && Order[Int].gteqv(aa,a), Some(a))
                 }
               })._1


  }

  property("insert isn't lossy") = forAll { xs: Lst[Int] =>
    val t = RedBlackTree.fromLst(xs)
    xs.forall(t member _)
  }

  property("union") = forAll { (xs: List[Int], ys: List[Int]) =>
    val std = xs.toSet union ys.toSet
    val l1 = RedBlackTree.fromLst(Lst.fromIterable(xs))
    val l2 = RedBlackTree.fromLst(Lst.fromIterable(ys))

    val ours = l1 union l2

    std === ours.toLst.toList.toSet
  }
}
