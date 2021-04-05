package cats.collections
package tests

import cats._
import cats.tests.CatsSuite
import cats.collections.arbitrary._
import cats.laws.discipline._

class MapSpec extends CatsSuite {
  def fromSet[K: Order,V](s: Set[(K,V)]): AvlMap[K,V] =
    s.foldLeft[AvlMap[K,V]](AvlMap.empty)(_ + _)

  def fromSetS[K: Order,V](s: Set[(K,V)]): Map[K,V] =
    s.foldLeft[Map[K,V]](Map.empty)(_ + _)

  def toSet[K: Order,V](m: AvlMap[K,V]): Set[(K,V)] =
    m.foldLeft[Set[(K,V)]](Set.empty)(_ + _)

  test("we can add things to a Map, then find them")(forAll {(xs: Set[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSetS(xs)

    xs.forall {
      case (k,_) => m.containsKey(k) && (m.get(k) == m2.get(k))
    } should be (true)
  })

  test("we can add things to a Map, then remove them")(forAll {(xs: Set[((String,Int), Boolean)]) =>
    val n = fromSet(xs.map(_._1))
    val n2 = fromSetS(xs.map(_._1))

    val m = xs.foldLeft[AvlMap[String,Int]](n)((mm,kvr) =>
      if(kvr._2) mm.remove(kvr._1._1) else mm
    )

    val m2 = xs.foldLeft[Map[String,Int]](n2)((mm,kvr) =>
      if(kvr._2) mm - (kvr._1._1) else mm
    )

    xs.forall {
      case (k,_) =>
        m.get(k._1) == m2.get(k._1)
    } should be (true)
  })

  test("we can remove things from a Map with alter")(forAll { (xs: Set[(String, Int)]) =>
    val m = fromSet(xs)

    xs.foldLeft(m) { case (r, (k, _)) => r.alter(k)(_ => None) }.set.isEmpty should be (true)
  })

  test("we can combine maps")(forAll {(xs: Set[(String,Int)],xs2: Set[(String,Int)]) =>
    val m = fromSet(xs)
    val m2 = fromSet(xs2)

    val sm = fromSetS(xs)
    val sm2 = fromSetS(xs2)

    toSet(m ++ m2) should contain theSameElementsAs (sm ++ sm2).toSet
  })

  test("map works")(forAll {(xs: Set[(String,Int)]) =>
    val f: Int => Int = _ + 1
    val f2: ((String,Int)) => (String,Int) = kv => kv._1 -> (kv._2 + 1)
    val m = fromSet(xs)
    val sm = fromSetS(xs)

    toSet(m map f) should contain theSameElementsAs (sm map f2).toSet
  })

  test("flatMap works")(forAll {(xs : Set[(String,Int)]) =>
    val f: Int => AvlMap[String,Int] = _ => fromSet(xs) map (_ + 1)
    val f2: ((String,Int)) => Set[(String,Int)] = kv => Set(kv._1 -> (kv._2 + 1))
    val m = fromSet(xs)
    val sm = fromSetS(xs)
    toSet(m flatMap f) should contain theSameElementsAs (sm flatMap f2).toSet
  })
}


class MapShow extends CatsSuite {

  test("show empty") {
    val map = AvlMap.empty[Int, Int]

    map.show should be("{}")
  }

  test("show mappings") {
    val map = AvlMap.empty[Int, Int].+((1, 2)).+((2, 3))

    map.show should be("{[1-->2]\n[2-->3]\n}")
  }
}

class MapLaws extends CatsSuite with ArbitrarySet with ArbitraryMap  {
  implicit val iso: SemigroupalTests.Isomorphisms[Map[String, *]] = SemigroupalTests.Isomorphisms.invariant[Map[String, *]]
  checkAll("Map[String,A]", FlatMapTests[Map[String,*]].flatMap[(String,Int),(String,Int),(String,Int)])
}
