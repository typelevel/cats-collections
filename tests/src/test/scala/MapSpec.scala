package dogs
package tests

import Predef._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import org.scalatest._
import dogs.tests.arbitrary._
import algebra.Order
import algebra.std.int._
import algebra.std.string._

object MapSpec extends Properties("Map") with ArbitraryList {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  def fromSet[K: Order,V](s: SSet[(K,V)]): Map[K,V] = 
    s.foldLeft[Map[K,V]](Map.empty)(_ + _)

  def fromSetS[K: Order,V](s: SSet[(K,V)]): MMap[K,V] = 
    s.foldLeft[MMap[K,V]](MMap.empty)(_ + _)

  def toSet[K: Order,V](m: Map[K,V]): SSet[(K,V)] =
    m.foldLeft[SSet[(K,V)]](SSet.empty)(_ + _)

  property("we can add things to a Map, then find them") = forAll {(xs: SSet[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSetS(xs)

    xs.forall {
      case (k,v) => m.containsKey(k) && (m.get(k).toScalaOption == m2.get(k))
    }
  }

  property("we can add things to a Map, then remove them") = forAll {(xs: SSet[((String,Int), Boolean)]) =>
    val n = fromSet(xs.map(_._1))
    val n2 = fromSetS(xs.map(_._1))

    val m = xs.foldLeft[Map[String,Int]](n)((mm,kvr) =>
      if(kvr._2) mm.remove(kvr._1._1) else mm
    )

    val m2 = xs.foldLeft[MMap[String,Int]](n2)((mm,kvr) =>
      if(kvr._2) mm - (kvr._1._1) else mm
    )

    xs.forall {
      case (k,v) =>
        m.get(k._1).toScalaOption == m2.get(k._1)
    }
  }

  property("we can combine maps") = forAll {(xs: SSet[(String,Int)],xs2: SSet[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSet(xs2)

    val sm = fromSetS(xs)
    val sm2 = fromSetS(xs2)

    toSet(m ++ m2) == (sm ++ sm2).to[SSet]
  }

  property("map works") = forAll {(xs: SSet[(String,Int)]) =>
    val f: Int => Int = _ + 1
    val f2: ((String,Int)) => (String,Int) = kv => kv._1 -> (kv._2 + 1)
    val m = fromSet(xs)
    val sm = fromSetS(xs)

    toSet(m map f) == (sm map f2).to[SSet]
  }
}
