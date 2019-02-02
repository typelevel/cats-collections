package cats.collections

import cats.implicits._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}
import Arbitrary.{arbitrary => arb}

object BitSetTest extends Properties("BitSet") {

  implicit val arbBitSet: Arbitrary[BitSet] =
    Arbitrary {
      val recur = Gen.lzy(arbBitSet.arbitrary)

      def onPair(fn: (BitSet, BitSet) => BitSet): Gen[BitSet] =
        for {
          a <- recur
          b <- recur
        } yield fn(a, b)

      Gen.frequency(
        (5, arb[List[Int]].map(xs => BitSet(xs: _*))),
        (5, Gen.sized { max => arb[Int].map { init => BitSet((init until (init + max)): _*) } }),
        (1, onPair( _ | _)),
        (1, onPair( _ & _))
      )
    }

  property("limit/height consistency") =
    forAll { (x: BitSet) =>
      import x.{limit, offset, height}
      (limit == (offset + (1L << (5 * height + 11)))) && (limit > offset)
    }

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
    forAll { (ns: Set[Int]) =>
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
      // This is only approximately true, but failures are very rare,
      // and without something like this its easy to end up with real
      // hashing bugs.
      (x == y) == (x.## == y.##)
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

  property("!x.isEmpty == x.nonEmpty") =
    forAll { (x: BitSet) =>
      x.nonEmpty == (!x.isEmpty)
    }

  property("BitSet.empty contains nothing") =
    forAll { (x: Int) =>
      !BitSet.empty(x)
    }

  property("x.iterator.forall(x(_))") =
    forAll { (x: BitSet) =>
      x.iterator.forall(x(_))
    }

  property("(x + a)(a)") =
    forAll { (x: BitSet, a: Int) =>
      val y = x + a
      y(a) :| s"$y(${a})"
    }

  property("!(x - a)(a)") =
    forAll { (x: BitSet, a: Int) =>
      !(x - a)(a)
    }

  property("x + a - a == x - a") =
    forAll { (x: BitSet, a: Int) =>
      ((x + a) - a) == (x - a)
    }

  property("x + a + a = x + a") =
    forAll { (x: BitSet, a: Int) =>
      val once = x + a
      (once + a) == once
    }

  property("x - a - a = x - a") =
    forAll { (x: BitSet, a: Int) =>
      val once = x - a
      (once - a) == once
    }

  property("x.toSet + a == (x + a).toSet") =
    forAll { (x: BitSet, a: Int) =>
      x.toSet + a == (x + a).toSet
    }

  property("x.toSet - a == (x - a).toSet") =
    forAll { (x: BitSet, a: Int) =>
      x.toSet - a == (x - a).toSet
    }

  property("+ is commutative") =
    forAll { (ns: List[Int]) =>
      BitSet(ns: _*) == BitSet(ns.reverse: _*)
    }

  property("- is commutative") =
    forAll { (x: BitSet, ns: List[Int]) =>
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

  property("(x | y)(z) == x(z) || y(z)") =
    forAll { (x: BitSet, y: BitSet, z: Int) =>
      def law(z: Int): Boolean = (x | y)(z) == (x(z) || y(z))

      law(z) && x.iterator.forall(law) && y.iterator.forall(law)
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

  property("(x & y)(z) == x(z) && y(z)") =
    forAll { (x: BitSet, y: BitSet, z: Int) =>
      def law(z: Int): Boolean = (x & y)(z) == (x(z) && y(z))

      law(z) && x.iterator.forall(law) && y.iterator.forall(law)
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

  property("((x ^ y) ^ z) = (x ^ (y ^ z))") =
    forAll { (x: BitSet, y: BitSet, z: BitSet) =>
      val lhs = ((x ^ y) ^ z)
      val rhs = (x ^ (y ^ z))
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x ^ y) = (y ^ x)") =
    forAll { (x: BitSet, y: BitSet) =>
      val lhs = (x ^ y)
      val rhs = (y ^ x)
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x ^ x) = 0") =
    forAll { (x: BitSet) =>
      val lhs = (x ^ x)
      val rhs = BitSet.empty
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x ^ 0) = x") =
    forAll { (x: BitSet) =>
      val lhs = (x ^ BitSet.empty)
      val rhs = x
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x ^ y) = ((x -- (x & y)) | (y -- (x & y)))") =
    forAll { (x: BitSet, y: BitSet) =>
      val xy = x & y
      val lhs = x ^ y
      val rhs = (x -- xy) | (y -- xy)
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x -- y).toSet = (x.toSet -- y.toSet)") =
    forAll { (x: BitSet, y: BitSet) =>
      val lhs = (x -- y).toSet
      val rhs = x.toSet -- y.toSet
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x -- x = 0") =
    forAll { (x: BitSet) =>
      val lhs = x -- x
      val rhs = BitSet.empty
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("0 -- x = 0") =
    forAll { (x: BitSet) =>
      val lhs = BitSet.empty -- x
      val rhs = BitSet.empty
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x -- BitSet(n) = x - n") =
    forAll { (x: BitSet, n: Int) =>
      val lhs = x -- BitSet(n)
      val rhs = x - n
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x -- 0 = x") =
    forAll { (x: BitSet) =>
      val lhs = x -- BitSet.empty
      (lhs == x) :| s"$lhs == $x"
    }

  property("x -- y -- y = x -- y") =
    forAll { (x: BitSet, y: BitSet) =>
      val lhs = x -- y -- y
      val rhs = x -- y
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("test order") =
    forAll { (x: BitSet, y: BitSet) =>
      val lhs = x compare y
      val rhs = x.iterator.toList compare y.iterator.toList
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("test ordering") =
    forAll { (x: BitSet, y: BitSet) =>
      val lhs = BitSet.orderingForBitSet.compare(x, y)
      val rhs = x.iterator.toList compare y.iterator.toList
      (lhs == rhs) :| s"$lhs == $rhs"
    }

}
