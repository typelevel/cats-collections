package cats.collections

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, /*Cogen, */Gen, Properties, Test}
import Arbitrary.{arbitrary => arb}

object BitSetTest extends Properties("BitSet") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)

  case class N(value: Int) {
    override def toString: String = value.toString
  }

  implicit val arbN: Arbitrary[N] =
    Arbitrary(Gen.choose(0, Int.MaxValue).map(N(_)))

  def build(xs: List[N]): BitSet =
    xs.foldLeft(BitSet.empty)((bs, n) => bs + n.value)

  implicit val arbBitSet: Arbitrary[BitSet] =
    Arbitrary(arb[List[N]].map(xs => BitSet(xs.map(_.value): _*)))

  property("(x = y) = (x.toSet = y.toSet)") =
    forAll { (x: BitSet, y: BitSet) =>
      val xs = x.toSet
      val ys = y.toSet
      ((x == y) == (xs == ys)) :| s"($x == $y) == ($xs == $ys)"
    }

  property("x.toSet == x.iterator.toSet") =
    forAll { (x: BitSet) =>
      (x.toSet == x.iterator.toSet) :| s"$x toSet == iterator.toSet"
    }

  property("BitSet(set: _*).toSet = set") =
    forAll { (ns0: Set[N]) =>
      val ns = ns0.map(_.value)
      val x = BitSet(ns.toList: _*)
      x.toSet == ns && ns.forall(x(_))
    }

  property("BitSet(x.toSet: _*) = x") =
    forAll { (x: BitSet) =>
      val y = BitSet(x.iterator.toList: _*)
      x == y
    }

  property("x.iterator.size = x.size") =
    forAll { (x: BitSet) =>
      x.iterator.size == x.size
    }

  property("(x = y) = (x.## = y.##)") =
    forAll { (x: BitSet, y: BitSet) =>
      (x == y) == (x.## == y.##) // only approximately true
    }

  property("x.compact = x") =
    forAll { (x: BitSet) =>
      x.compact == x
    }
  property("x.isEmpty == (x.compact eq BitSet.Empty)") =
    forAll { (x: BitSet) =>
      (x.isEmpty == (x.compact eq BitSet.Empty)) :| s"$x isEmpty but not compact to Empty"
    }

  property("x.isEmpty = (x.size = 0)") =
    forAll { (x: BitSet) =>
      x.isEmpty == (x.size == 0)
    }

  property("x.iterator.forall(x(_))") =
    forAll { (x: BitSet) =>
      x.iterator.forall(x(_))
    }

  property("(x + a)(a)") =
    forAll { (x: BitSet, a: N) =>
      val y = x + a.value
      y(a.value) :| s"$y(${a.value})"
    }

  property("!(x - a)(a)") =
    forAll { (x: BitSet, a: N) =>
      !(x - a.value)(a.value)
    }

  property("x + a - a == x - a") =
    forAll { (x: BitSet, a: N) =>
      ((x + a.value) - a.value) == (x - a.value)
    }

  property("x + a + a = x + a") =
    forAll { (x: BitSet, a: N) =>
      val once = x + a.value
      (once + a.value) == once
    }

  property("x - a - a = x - a") =
    forAll { (x: BitSet, a: N) =>
      val once = x - a.value
      (once - a.value) == once
    }

  property("x.toSet + a == (x + a).toSet") =
    forAll { (x: BitSet, a: N) =>
      x.toSet + a.value == (x + a.value).toSet
    }

  property("x.toSet - a == (x - a).toSet") =
    forAll { (x: BitSet, a: N) =>
      x.toSet - a.value == (x - a.value).toSet
    }

  property("+ is commutative") =
    forAll { (ns0: List[N]) =>
      val ns = ns0.map(_.value)
      BitSet(ns: _*) == BitSet(ns.reverse: _*)
    }

  property("- is commutative") =
    forAll { (x: BitSet, ns0: List[N]) =>
      val ns = ns0.map(_.value)
      ns.foldLeft(x)(_ - _) == ns.reverse.foldLeft(x)(_ - _)
    }

  property("x | x = x") =
    forAll { (x: BitSet) =>
      (x | x) == x
    }

  property("x | Empty = x") =
    forAll { (x: BitSet) =>
      val y = x | BitSet.empty
      (y == x) :| s"$y ==\n$x"
    }

  property("x | y = y | x") =
    forAll { (x: BitSet, y: BitSet) =>
      try {
        val lhs = x | y
        val rhs = y | x
        (lhs == rhs) :| s"$lhs == $rhs"
      } catch { case (e: Throwable) => e.printStackTrace; throw e }
    }

  property("(x | y) | z = x | (y | z)") =
    forAll { (x: BitSet, y: BitSet, z: BitSet) =>
      try {
        val lhs = ((x | y) | z).compact
        val rhs = (x | (y | z)).compact
        (lhs == rhs) :| s"$lhs == $rhs"
      } catch { case (e: Throwable) => e.printStackTrace; throw e }
    }

  property("x & x = x") =
    forAll { (x: BitSet) =>
      val y = x & x
      (y == x) :| s"$y ==\n$x"
    }

  property("x & Empty = Empty") =
    forAll { (x: BitSet) =>
      (x & BitSet.empty) == BitSet.empty
    }

  property("x & y = y & x") =
    forAll { (x: BitSet, y: BitSet) =>
      (x & y) == (y & x)
    }

  property("(x & y) & z = x & (y & z)") =
    forAll { (x: BitSet, y: BitSet, z: BitSet) =>
      ((x & y) & z) == (x & (y & z))
    }

  property("(x & (y | z) = (x & y) | (x & z)") =
    forAll { (x: BitSet, y: BitSet, z: BitSet) =>
      val lhs = x & (y | z)
      val rhs = (x & y) | (x & z)
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x.iterator.toList.reverse = x.reverseIterator.toList") =
    forAll { (x: BitSet) =>
      val lhs = x.iterator.toList.reverse
      val rhs = x.reverseIterator.toList
      (lhs == rhs) :| s"$lhs == $rhs"
    }
}
