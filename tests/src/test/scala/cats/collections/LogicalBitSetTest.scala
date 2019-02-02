package cats.collections

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import Arbitrary.{arbitrary => arb}

object LogicalBitSetTest extends Properties("LogicalBitSet") {

  implicit val arbLogicalBitSet: Arbitrary[LogicalBitSet] =
    Arbitrary(arb[Either[List[Int], List[Int]]].map {
      case Left(xs) => LogicalBitSet.Present(BitSet(xs.take(3).map(_ & 0xffff): _*))
      case Right(xs) => LogicalBitSet.Absent(BitSet(xs.take(3).map(_ & 0xffff): _*))
    })

  property("(x = x)") =
    forAll { (x: LogicalBitSet) =>
      x == x
    }

  property("(x + a)(a)") =
    forAll { (x: LogicalBitSet, a: Int) =>
      val y = x + a
      y(a) :| s"$y(${a})"
    }

  property("!(x - a)(a)") =
    forAll { (x: LogicalBitSet, a: Int) =>
      !(x - a)(a)
    }

  property("x + a - a == x - a") =
    forAll { (x: LogicalBitSet, a: Int) =>
      ((x + a) - a) == (x - a)
    }

  property("x + a + a = x + a") =
    forAll { (x: LogicalBitSet, a: Int) =>
      val once = x + a
      (once + a) == once
    }

  property("x - a - a = x - a") =
    forAll { (x: LogicalBitSet, a: Int) =>
      val once = x - a
      (once - a) == once
    }

  property("+ is commutative") =
    forAll { (ns: List[Int]) =>
      LogicalBitSet(ns: _*) == LogicalBitSet(ns.reverse: _*)
    }

  property("- is commutative") =
    forAll { (x: LogicalBitSet, ns: List[Int]) =>
      ns.foldLeft(x)(_ - _) == ns.reverse.foldLeft(x)(_ - _)
    }

  property("x | x = x") =
    forAll { (x: LogicalBitSet) =>
      (x | x) == x
    }

  property("x | Empty = x") =
    forAll { (x: LogicalBitSet) =>
      val y = x | LogicalBitSet.empty
      (y == x) :| s"$y ==\n$x"
    }

  property("x | y = y | x") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      try {
        val lhs = x | y
        val rhs = y | x
        (lhs == rhs) :| s"$lhs == $rhs"
      } catch { case (e: Throwable) => e.printStackTrace; throw e }
    }

  property("(x | y) | z = x | (y | z)") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet, z: LogicalBitSet) =>
      try {
        val lhs = ((x | y) | z)
        val rhs = (x | (y | z))
        (lhs == rhs) :| s"$lhs == $rhs"
      } catch { case (e: Throwable) => e.printStackTrace; throw e }
    }

  property("x & x = x") =
    forAll { (x: LogicalBitSet) =>
      val y = x & x
      (y == x) :| s"$y ==\n$x"
    }

  property("x & Empty = Empty") =
    forAll { (x: LogicalBitSet) =>
      (x & LogicalBitSet.empty) == LogicalBitSet.empty
    }

  property("x & y = y & x") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      (x & y) == (y & x)
    }

  property("(x & y) & z = x & (y & z)") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet, z: LogicalBitSet) =>
      ((x & y) & z) == (x & (y & z))
    }

  property("(x & (y | z) = (x & y) | (x & z)") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet, z: LogicalBitSet) =>
      val lhs = x & (y | z)
      val rhs = (x & y) | (x & z)
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("((x ^ y) ^ z) = (x ^ (y ^ z))") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet, z: LogicalBitSet) =>
      val lhs = ((x ^ y) ^ z)
      val rhs = (x ^ (y ^ z))
      (lhs == rhs) :| s"$lhs == $rhs"
    }

/*

! LogicalBitSet.(x & (y | z) = (x & y) | (x & z): Falsified after 93 passed tests.

Absent(BitSet(), Leaf(0, {})) == Absent(BitSet(0), Branch(0, 1, Array(0 -> Leaf(0, {0 -> 1}))))

ARG_0: Absent(BitSet(0), Leaf(0, {0 -> 1}))

ARG_1: Absent(BitSet(0, 43646), Branch(0, 1, Array(0 -> Leaf(0, {0 -> 1}), 21 -> Leaf(43008, {9 -> 4611686018427387904}))))

ARG_2: Absent(BitSet(8526, 65535), Branch(0, 1, Array(0 -> Leaf(0, {}), 4 -> Leaf(8192, {5 -> 16384}), 31 -> Leaf(63488, {31 -> -9223372036854775808}))))

---

ARG_0: Absent(BitSet(0), Leaf(0, {0 -> 1}))

ARG_1: Absent(BitSet(0, 43646), Branch(0, 1, Array(0 -> Leaf(0, {0 -> 1}), 21 -> Leaf(43008, {9 -> 4611686018427387904}))))

ARG_2: Absent(BitSet(8526, 65535), Branch(0, 1, Array(0 -> Leaf(0, {}), 4 -> Leaf(8192, {5 -> 16384}), 31 -> Leaf(63488, {31 -> -9223372036854775808}))))

 */

  property("regression #999") =
    forAll { (b: Byte) =>
      import LogicalBitSet._
      // val x = Absent(BitSet(0))
      // val y = Present(BitSet(0, 1, 2112486950, 2147483647, -2147483648, -1))
      // val z = Absent(BitSet(0, 329654831, 2147483647))
      val x = Absent(BitSet(0))
      val y = Absent(BitSet(0, 43646))
      val z = Absent(BitSet(8526, 65535))
      // println(s"x = $x")
      // println(s"y = $y")
      // println(s"z = $z")
      val lhs = x & (y | z)
      val rhs = (x & y) | (x & z)
      (lhs == rhs) //:| s"$lhs == $rhs"
    }

  property("(x ^ y) = (y ^ x)") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      val lhs = (x ^ y)
      val rhs = (y ^ x)
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("(x ^ x) = 0") =
    forAll { (x: LogicalBitSet) =>
      val lhs = (x ^ x)
      val rhs = LogicalBitSet.empty
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x - x = 0") =
    forAll { (x: LogicalBitSet) =>
      val lhs = x -- x
      val rhs = LogicalBitSet.empty
      (lhs == rhs) :| s"$lhs == $rhs"
    }


  property("x - y - y = x - y") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      val lhs = x -- y -- y
      val rhs = x -- y
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("x - y - z = x - z - y") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet, z: LogicalBitSet) =>
      val lhs = x -- y -- z
      val rhs = x -- z -- y
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  // property("(x ^ y) = ((x -- (x & y)) | (y -- (x & y)))") =
  //   forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
  //     val xy = x & y
  //     val lhs = x ^ y
  //     val rhs = (x -- xy) | (y -- xy)
  //     (lhs == rhs) :| s"$lhs == $rhs"
  //   }

  property("~(x | y) = ~x & ~y") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      val lhs = ~(x | y)
      val rhs = ~x & ~y
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("~(x & y) = ~x | ~y") =
    forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
      val lhs = ~(x & y)
      val rhs = ~x | ~y
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  property("~~x = x") =
    forAll { (x: LogicalBitSet) =>
      val lhs = ~(~x)
      val rhs = x
      (lhs == rhs) :| s"$lhs == $rhs"
    }

  // property("x -- y = x & (~y)") =
  //   forAll { (x: LogicalBitSet, y: LogicalBitSet) =>
  //     val lhs = x -- y
  //     val rhs = x & ~y
  //     (lhs == rhs) :| s"$lhs == $rhs"
  //   }
}
