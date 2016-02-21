package dogs

import dogs.Predef._
import dogs.std.intOrder
import dogs.tests.arbitrary._
import dogs.syntax.birds._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import scala.Array
import scala.Predef.intArrayOps

object SetSpec extends Properties("Set") with ArbitraryList {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  property("set is always sorted") = forAll { (xs: List[Int]) =>
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

  property("set is always balanced") = forAll { (xs: List[Int]) =>
    val tree = Set(xs.toScalaList: _*)
    balanced(tree)
  }

  property("set can delete") = forAll{ (xs: MMap[Int,Boolean]) =>
    val tree = Set(xs.keySet.toSeq: _*)
    val filtered = xs.foldLeft(tree)((t,i) => if(i._2) t else t.remove(i._1))

    val ours: SSet[Int] = filtered.toList.toScalaList.to[SSet]
    val theirs: SSet[Int] = xs.collect{ case (i, true) => i }.to[SSet]

    (ours == theirs) && balanced(filtered)
  }

  property("contains works") = forAll{ (xs: MMap[Int,Boolean]) =>
    val tree = xs.foldLeft[Set[Int]](Set.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.contains(k) == v
    }.foldLeft(true)(_ && _)
  }

  property("find works") = forAll{ (xs: MMap[Int,Boolean]) =>
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

    (xt intersect yt).toScalaSet == (xs intersect ys) && (xt & yt).toScalaSet == (xs intersect ys)

  }

  property("union is correct") = forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt union yt).toScalaSet == (xs union ys) && (xt | yt).toScalaSet == (xs union ys) 

  }

  property("we can take the difference of sets") = forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt diff yt).toScalaSet == (xs diff ys) && (xt - yt).toScalaSet == (xs diff ys)
  }

  property("map works") = forAll{ (xs: SSet[Int]) =>
    val f: Int => Int = _ + 1

    val xt = Set(xs.toSeq: _*)

    (xt map f).toScalaSet == (xs map f)
  }
}

