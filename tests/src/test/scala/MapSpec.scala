package dogs
package tests

import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import org.scalatest._
import dogs.tests.arbitrary._
import dogs.std.stringOrder

object MapSpec extends Properties("Map") with ArbitraryList {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  property("we can add things to a Map, then find them") = forAll {(xs: SSet[(String,Int)]) =>
    val m = xs.foldLeft[Map[String,Int]](Map.empty)(_ + _)
    val m2 = xs.foldLeft[MMap[String,Int]](MMap.empty)(_ + _)

    xs.forall {
      case (k,v) => m.containsKey(k) && (m.get(k).toScalaOption == m2.get(k))
    }
  }

  property("we can add things to a Map, then remove them") = forAll {(xs: SSet[((String,Int), Boolean)]) =>
    val n = xs.foldLeft[Map[String,Int]](Map.empty)(_ + _._1)
    val n2 = xs.foldLeft[MMap[String,Int]](MMap.empty)(_ + _._1)

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
}
