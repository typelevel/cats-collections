package cats.collections
package tests

import cats.tests.CatsSuite
import cats.implicits._

class SetSpec extends CatsSuite {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  test("set is always sorted")(forAll { (xs: List[Int]) =>
    val tree = xs.foldLeft(Set.empty[Int])(_ + _)

    val ours: List[Int] = tree.toList
    val theirs: List[Int] = xs.to[SSet].to[Array].sorted.to[List]

    ours should be (theirs)
  })

  import Set._
  def balanced[A](t: Set[A]): Boolean = t match {
    case BTNil() => true
    case Branch(_, l, r) =>
      java.lang.Math.abs(l.height - r.height) <= 1 && balanced(l) && balanced(r)

  }

  test("set is always balanced")(forAll { (xs: List[Int]) =>
    val tree = Set(xs: _*)
    balanced(tree) should be(true)
  })

  test("set can delete")(forAll{ (xs: MMap[Int,Boolean]) =>
    val tree = Set(xs.keySet.toSeq: _*)
    val filtered = xs.foldLeft(tree)((t,i) => if(i._2) t else t.remove(i._1))

    val ours: SSet[Int] = filtered.toList.to[SSet]
    val theirs: SSet[Int] = xs.collect{ case (i, true) => i }.to[SSet]

    ours should be (theirs)
    balanced(filtered) should be(true)
  })

  test("contains works")(forAll{ (xs: MMap[Int,Boolean]) =>
    val tree = xs.foldLeft[Set[Int]](Set.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.contains(k) == v
    }.foldLeft(true)(_ && _) should be(true)
  })

  test("find works")(forAll{ (xs: MMap[Int,Boolean]) =>
    val tree = xs.foldLeft[Set[Int]](Set.empty)((t,i) =>
      if(i._2) t + i._1 else t
    )

    xs.map{
      case (k,v) => tree.find(_ == k).isDefined == v
    }.foldLeft(true)(_ && _) should be(true)
  })

  test("intersect is correct")(forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt intersect yt).toScalaSet should be (xs intersect ys)
    (xt & yt).toScalaSet should be(xs intersect ys)
  })

  test("union is correct")(forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt union yt).toScalaSet should be(xs union ys)
    (xt | yt).toScalaSet should be(xs union ys)
  })

  test("we can take the difference of sets")(forAll{ (xs: SSet[Int], ys: SSet[Int]) =>
    val xt = Set(xs.toSeq: _*)
    val yt = Set(ys.toSeq: _*)

    (xt diff yt).toScalaSet should be(xs diff ys)
    (xt - yt).toScalaSet should be(xs diff ys)
  })

  test("map works") (forAll{ (xs: SSet[Int]) =>
    val f: Int => Int = _ + 1

    val xt = Set(xs.toSeq: _*)

    (xt map f).toScalaSet should be(xs map f)
  })
}

