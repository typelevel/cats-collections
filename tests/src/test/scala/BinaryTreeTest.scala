package dogs
package tests

import dogs.Predef._
import dogs.std.intOrder
import dogs.tests.arbitrary._
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary, _}
import org.scalacheck.Prop._
import org.scalacheck._

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
    val tree = BinaryTree(xs.toScalaList: _*)
    balanced(tree)
  }

  property("binary tree can delete") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = BinaryTree(xs.keySet.toSeq: _*)
    val filtered = xs.foldLeft(tree)((t,i) => if(i._2) t else t.remove(i._1))

    val ours: Set[Int] = filtered.toList.toScalaList.to[Set]
    val theirs: Set[Int] = xs.collect{ case (i, true) => i }.to[Set]

    (ours == theirs) && balanced(filtered)
  }

  property("contains works") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = xs.foldLeft[BinaryTree[Int]](BinaryTree.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.contains(k) == v
    }.foldLeft(true)(_ && _)
  }

  property("find works") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = xs.foldLeft[BinaryTree[Int]](BinaryTree.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.find(_ == k).isDefined == v
    }.foldLeft(true)(_ && _)
  }
}

