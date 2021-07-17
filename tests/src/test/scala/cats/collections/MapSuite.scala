package cats.collections

import cats._
import cats.collections.arbitrary._
import cats.laws.discipline._
import cats.syntax.show._
import munit.{DisciplineSuite, FunSuite}
import org.scalacheck.Prop._

import scala.collection.immutable.SortedSet

class MapSuite extends DisciplineSuite {
  def fromSet[K: Order,V](s: Set[(K,V)]): AvlMap[K,V] =
    s.foldLeft[AvlMap[K,V]](AvlMap.empty)(_ + _)

  def fromSetS[K: Order,V](s: Set[(K,V)]): Map[K,V] =
    s.foldLeft[Map[K,V]](Map.empty)(_ + _)

  def toSet[K: Order: Ordering, V: Ordering](m: AvlMap[K,V]): Set[(K,V)] =
    m.foldLeft[Set[(K,V)]](SortedSet.empty[(K, V)])(_ + _)

  test("we can add things to a Map, then find them")(forAll {(xs: Set[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSetS(xs)

    assert(xs.forall {
      case (k,_) => m.containsKey(k) && (m.get(k) == m2.get(k))
    })
  })

  test("we can add things to a Map, then remove them")(forAll { (xs: Set[((String,Int), Boolean)]) =>
    val n = fromSet(xs.map(_._1))
    val n2 = fromSetS(xs.map(_._1))

    val m = xs.foldLeft[AvlMap[String,Int]](n)((mm,kvr) =>
      if(kvr._2) mm.remove(kvr._1._1) else mm
    )

    val m2 = xs.foldLeft[Map[String,Int]](n2)((mm,kvr) =>
      if(kvr._2) mm - (kvr._1._1) else mm
    )

    assert(xs.forall {
      case (k,_) =>
        m.get(k._1) == m2.get(k._1)
    })
  })

  test("we can remove things from a Map with alter")(forAll { (xs: Set[(String, Int)]) =>
    val m = fromSet(xs)

    assert(xs.foldLeft(m) { case (r, (k, _)) => r.alter(k)(_ => None) }.set.isEmpty)
  })

  test("we can combine maps")(forAll {(xs: Set[(String,Int)],xs2: Set[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSet(xs2)

    val sm = fromSetS(xs)
    val sm2 = fromSetS(xs2)

    assertEquals(toSet(m ++ m2), SortedSet((sm ++ sm2).toSeq: _*))
  })

  test("map works")(forAll {(xs: Set[(String,Int)]) =>
    val f: Int => Int = _ + 1
    val f2: ((String,Int)) => (String,Int) = kv => kv._1 -> (kv._2 + 1)
    val m = fromSet(xs)
    val sm = fromSetS(xs)

    assertEquals(toSet(m map f), SortedSet((sm map f2).toSeq: _*))
  })

  test("flatMap works")(forAll {(xs : Set[(String,Int)]) =>
    val f: Int => AvlMap[String,Int] = _ => fromSet(xs) map (_ + 1)
    val f2: ((String,Int)) => Set[(String,Int)] = kv => Set(kv._1 -> (kv._2 + 1))
    val m = fromSet(xs)
    val sm = fromSetS(xs)

    assertEquals(toSet(m flatMap f), SortedSet((sm flatMap f2).toSeq: _*))
  })
}


class MapShowSuite extends FunSuite {
  test("show empty") {
    val map = AvlMap.empty[Int, Int]

    assertEquals(map.show, "{}")
  }

  test("show mappings") {
    val map = AvlMap.empty[Int, Int].+((1, 2)).+((2, 3))

    assertEquals(map.show, "{[1-->2]\n[2-->3]\n}")
  }
}

class MapLawsSuite extends DisciplineSuite with ArbitrarySet with ArbitraryMap  {
  implicit val iso: SemigroupalTests.Isomorphisms[Map[String, *]] =
    SemigroupalTests.Isomorphisms.invariant[Map[String, *]]

  checkAll("Map[String,A]", FlatMapTests[Map[String,*]].flatMap[(String,Int),(String,Int),(String,Int)])
}
