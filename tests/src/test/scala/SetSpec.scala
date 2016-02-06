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

object SetSpec extends Properties("Set") with ArbitraryList {
  import scala.collection.immutable.{Set => SSet}

  property("binary tree is always sorted") = forAll { (xs: List[Int]) =>
    val tree = xs.foldLeft(Set.empty[Int])(_ + _)

    val ours: scala.List[Int] = tree.toList.toScalaList
    val theirs: scala.List[Int] = xs.toScalaList.to[SSet].to[Array].sorted.to[scala.List]

    ours == theirs
  }

  import Set._
  def balanced[A](t: Set[A]): Boolean = t match {
    case BTNil() => true
    case Branch(_, l, r) =>
      java.lang.Math.abs(l.height - r.height) <= 1 && balanced(l) && balanced(r)

  }

  property("binary tree is always balanced") = forAll { (xs: List[Int]) =>
    val tree = Set(xs.toScalaList: _*)
    balanced(tree)
  }

  property("binary tree can delete") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = Set(xs.keySet.toSeq: _*)
    val filtered = xs.foldLeft(tree)((t,i) => if(i._2) t else t.remove(i._1))

    val ours: SSet[Int] = filtered.toList.toScalaList.to[SSet]
    val theirs: SSet[Int] = xs.collect{ case (i, true) => i }.to[SSet]

    (ours == theirs) && balanced(filtered)
  }

  property("contains works") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = xs.foldLeft[Set[Int]](Set.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.contains(k) == v
    }.foldLeft(true)(_ && _)
  }

  property("find works") = forAll{ (xs: Map[Int,Boolean]) =>
    val tree = xs.foldLeft[Set[Int]](Set.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.find(_ == k).isDefined == v
    }.foldLeft(true)(_ && _)
  }

  property("intersect is correct") = forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt intersect yt).toScalaSet == (xs intersect ys)
  }

  property("union is correct") = forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt union yt).toScalaSet == (xs union ys)
  }

  property("we can take the difference of sets") = forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt diff yt).toScalaSet == (xs diff ys)
  }
}

