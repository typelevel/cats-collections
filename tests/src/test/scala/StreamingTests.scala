package dogs
package tests

import dogs.syntax.streaming._
import dogs.tests.arbitrary.all._
import cats._
import cats.implicits._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.OrderLaws
import org.scalacheck._
import org.scalacheck.Cogen._
import scala.collection.immutable.{List => SList}
import scala.collection.immutable.Vector

class StreamingTests extends DogsSuite {
  import Streaming._
  // who oh why do these need to be here?
//  implicit val ilive: Cogen[ListWrapper[Int]] =
//    cogenFoldable[ListWrapper,Int](ListWrapper.foldable, implicitly)
//  implicit val ina: Cogen[Streaming[Int]] =
//    dogs.tests.arbitrary.cogen.cogenFoldable[Streaming,Int]
//  implicit val utopia: Cogen[Streaming[ListWrapper[Int]]] =
//    dogs.tests.arbitrary.cogen.cogenFoldable[Streaming, ListWrapper[Int]](Streaming.streamInstance, implicitly)

  checkAll("Streaming[Int]", CartesianTests[Streaming].cartesian[Int, Int, Int])
  checkAll("Cartesian[Streaming]", SerializableTests.serializable(Cartesian[Streaming]))

  checkAll("Streaming[Int]", CoflatMapTests[Streaming].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Streaming]", SerializableTests.serializable(CoflatMap[Streaming]))

  checkAll("Streaming[Int]", AlternativeTests[Streaming].alternative[Int, Int, Int])
  checkAll("Alternative[Streaming]", SerializableTests.serializable(Alternative[Streaming]))

  checkAll("Streaming[Int]", MonadTests[Streaming].monad[Int, Int, Int])
  checkAll("Monad[Streaming]", SerializableTests.serializable(Monad[Streaming]))

  checkAll("Streaming[Int] with Option", TraverseTests[Streaming].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Streaming]", SerializableTests.serializable(Traverse[Streaming]))

  checkAll("Streaming[Int]", OrderLaws[Streaming[Int]].order)
  checkAll("Order[Streaming[Int]]", SerializableTests.serializable(Order[Streaming[Int]]))

/*
  {
    implicit val I = ListWrapper.partialOrder[Int]
    checkAll("Streaming[ListWrapper[Int]]", OrderLaws[Streaming[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[Streaming[ListWrapper[Int]]]", SerializableTests.serializable(PartialOrder[Streaming[ListWrapper[Int]]]))
  }

  {
    implicit val I = ListWrapper.eqv[Int]
    checkAll("Streaming[ListWrapper[Int]]", OrderLaws[Streaming[ListWrapper[Int]]].eqv)
    checkAll("Eq[Streaming[ListWrapper[Int]]]", SerializableTests.serializable(Eq[Streaming[ListWrapper[Int]]]))
  }

 */
}
 
class AdHocStreamingTests extends DogsSuite {
  import Streaming._

//  implicit val ilive: Cogen[ListWrapper[Int]] =
//    cogenFoldable[ListWrapper,Int](ListWrapper.foldable, implicitly)
//  implicit val ina: Cogen[Streaming[Int]] =
//    dogs.tests.arbitrary.cogen.cogenFoldable[Streaming,Int]
//  implicit val utopia: Cogen[Streaming[ListWrapper[Int]]] =
//    dogs.tests.arbitrary.cogen.cogenFoldable[Streaming, ListWrapper[Int]](Streaming.streamInstance, implicitly)

  test("results aren't reevaluated after memoize") {
    forAll { (orig: Streaming[Int]) =>
      val ns = orig.toList
      val size = ns.foldLeft(0)((i, _) => i+1)

      var i = 0
      val memoized = orig.map { n => i += 1; n }.memoize

      val xs = memoized.toList
      i should === (size)
      xs should === (ns)

      val ys = memoized.toList
      i should === (size)
      ys should === (ns)
    }
  }

  test("fromList/toList") {
    forAll { (xs: List[Int]) =>
      Streaming.fromList(xs).toList should === (xs)
    }

    forAll { (xs: Streaming[Int]) =>
      val list = xs.toList
      Streaming.fromList(list).toList should === (list)
    }
  }

  test("map") {
    forAll { (xs: Streaming[Int], f: Int => Double) =>
      xs.map(f).toList should === (xs.toList.map(f))
    }
  }

  test("flatMap") {
    forAll { (xs: Streaming[Int], f: Int => Streaming[Double]) =>
      xs.flatMap(f).toList should === (xs.toList.flatMap(f(_).toList))
    }
  }

  test("filter") {
    forAll { (xs: Streaming[Int], f: Int => Boolean) =>
      xs.filter(f).toList should === (xs.toList.filter(f))
    }
  }

  test("foldLeft") {
    forAll { (xs: Streaming[String], n: Int, f: (Int, String) => Int) =>
      xs.foldLeft(n)(f) should === (xs.toList.foldLeft(n)(f))
    }
  }

  test("isEmpty") {
    forAll { (xs: Streaming[String], n: Int, f: (Int, String) => Int) =>
      xs.isEmpty should === (xs.toList.isEmpty)
    }
  }

  test("++") {
    forAll { (xs: Streaming[Int], ys: Streaming[Int]) =>
      (xs ++ ys).toList should === (xs.toList ::: ys.toList)
    }
  }

  test("zip") {
    forAll { (xs: Streaming[Int], ys: Streaming[Int]) =>
      (xs zip ys).toList should === (xs.toList zip ys.toList)
    }
  }

  test("zipWithIndex") {
    forAll { (xs: Streaming[Int]) =>
      xs.zipWithIndex.toList should === (xs.toList.zipWithIndex)
    }
  }

  test("unzip") {
    forAll { (xys: Streaming[(Int, Int)]) =>
      val (xs, ys): (Streaming[Int], Streaming[Int]) = xys.unzip
      val (ps, qs): (List[Int], List[Int]) = (xys.toList.unzip)

      xs.toList should === (ps)
      ys.toList should === (qs)
    }
  }

  test("exists") {
    forAll { (xs: Streaming[Int], f: Int => Boolean) =>
      xs.exists(f) should === (xs.toList.exists(f))
    }
  }

  test("forall") {
    forAll { (xs: Streaming[Int], f: Int => Boolean) =>
      xs.forall(f) should === (xs.toList.forall(f))
    }
  }

  test("take") {
    forAll { (xs: Streaming[Int], n: Int) =>
      xs.take(n).toList should === (xs.toList.take(n))
    }
  }

  test("drop") {
    forAll { (xs: Streaming[Int], n: Int) =>
      xs.drop(n).toList should === (xs.toList.drop(n))
    }
  }

  test("takeWhile") {
    forAll { (xs: Streaming[Int], f: Int => Boolean) =>
      xs.takeWhile(f).toList should === (xs.toList.takeWhile(f))
    }
  }

  test("dropWhile") {
    forAll { (xs: Streaming[Int], f: Int => Boolean) =>
      xs.dropWhile(f).toList should === (xs.toList.dropWhile(f))
    }
  }

  test("tails") {
    forAll { (xs: Streaming[Int]) =>
      xs.tails.map(_.toList).toList should === (xs.toList.tails.toList)
    }
  }

  test("toArray") {
    forAll { (xs: Streaming[Int]) =>
      xs.toArray should be (xs.toArray)
    }
  }

  test("compact should result in same values") {
    forAll { (xs: Streaming[Int]) =>
      Eq[Streaming[Int]].eqv(xs.compact, xs) should be(true)
//       should === (xs)
    }
  }

  test("fromIterable consistent with fromList") {
    forAll { (xs: SList[Int]) =>
      Streaming.fromIterable(xs) should === (Streaming.fromList(xs))
    }
  }

  test("fromIteratorUnsafe consistent with fromList") {
    forAll { (xs: SList[Int]) =>
      Streaming.fromIteratorUnsafe(xs.iterator) should === (Streaming.fromList(xs))
    }
  }

  test("continually consistent with List.fill") {
    forAll { (l: Long, b: Byte) =>
      val n = b.toInt
      Streaming.continually(l).take(n).toList should === (List.fill(n)(l))
    }
  }

  test("continually consistent with thunk") {
    forAll { (l: Long, b: Byte) =>
      val n = b.toInt
      Streaming.continually(l).take(n) should === (Streaming.thunk(() => l).take(n))
    }
  }

  test("equality consistent with list equality") {
    forAll { (xs: Streaming[Int], ys: Streaming[Int]) =>
      Eq[Streaming[Int]].eqv(xs, ys) should === (xs.toList == ys.toList)
    }
  }

  test("unfold with Some consistent with infinite") {
    forAll { (start: Int, b: Byte) =>
      val n = b.toInt
      val unfolded = Streaming.unfold(Some(start))(n => Some(n + 1)).take(n)
      unfolded should === (Streaming.infinite(start)(_ + 1).take(n))
    }
  }

  test("unfold consistent with infinite then takeWhile") {
    implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.choose(-10, 20))
    forAll { (start: Int, n: Int) =>
      val end = start + n
      def check(i: Int): Option[Int] = if (i <= end) Some(i) else None
      val unfolded = Streaming.unfold(check(start))(i => check(i + 1))
      val fromInfinite = Streaming.infinite(start)(_ + 1).takeWhile(_ <= end)
      unfolded.toList should === (fromInfinite.toList)
    }
  }

  test("unfold on None returns empty stream") {
    forAll { (f: Int => Option[Int]) =>
      Streaming.unfold[Int](None)(f) should === (Streaming.empty[Int])
    }
  }

  test("peekEmpty consistent with isEmpty") {
    forAll { (s: Streaming[Int]) =>
      s.peekEmpty.foreach(v => {v should  === (s.isEmpty) ;()} )
    }
  }

  test("memoize doesn't change values") {
    forAll { (s: Streaming[Int]) =>
      s.memoize should === (s)
    }
  }

  test("thunk is evaluated for each item") {
    // don't want the stream to be too big
    implicit val arbInt = Arbitrary(Gen.choose(-10, 20))
    forAll { (start: Int, end: Int) =>
      var i = start - 1
      val stream = Streaming.thunk{ () => i += 1; i}.takeWhile(_ <= end)
      stream.toList should === ((scala.Range(start, end+1).toList))
    }
  }

  test("uncons consistent with headOption"){
    forAll { (s: Streaming[Int]) =>
      s.uncons.map(_._1) should === (s.toList.headOption)
    }
  }

  test("uncons tail consistent with drop(1)"){
    forAll { (s: Streaming[Int]) =>
      val tail: Option[Streaming[Int]] = s.uncons.map(_._2.value)
      tail.foreach(v => { v.toList should === (s.toList.drop(1));()} )
    }
  }

  test("isEmpty consistent with fold"){
    forAll { (s: Streaming[Int]) =>
      s.isEmpty should === (s.fold(Now(true), (_, _) => false))
    }
  }

  implicitly[Arbitrary[(Int, cats.Eval[dogs.Streaming[Int]]) => dogs.Streaming[Long]]]

  implicitly[Arbitrary[(Int,Streaming[Int])]]
  implicitly[Cogen[Streaming[Int]]]
  implicitly[Cogen[Eval[Streaming[Int]]]]
  implicitly[Arbitrary[Eval[Int]]]

  test("foldStreaming consistent with fold"){
    forAll { (ints: Streaming[Int], longs: Streaming[Long], f: (Int, Eval[Streaming[Int]]) => Streaming[Long]) =>
      ints.foldStreaming(longs, f) should === (ints.fold(Now(longs), f))
    }
  }

  test("interval") {
    // we don't want this test to take a really long time
    implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.choose(-10, 20))
    forAll { (start: Int, end: Int) =>
      Streaming.interval(start, end).toList should === (scala.Range(start, end+1).toList)
    }
  }

  test("merge") {
    forAll { (xs: SList[Int], ys: SList[Int]) =>
      (Streaming.fromIterable(xs.sorted) merge Streaming.fromIterable(ys.sorted)).toList should === ((xs ::: ys).sorted)
    }
  }

  test("product") {
    forAll { (xs: Streaming[Int], ys: Streaming[Int]) =>
      val result = (xs product ys).iterator.toList.toSet
      val expected = (for { x <- xs; y <- ys } yield (x, y)).toList.toSet
      result should === (expected)
    }

    val nats = Streaming.from(1) // 1, 2, 3, ...

    def isRelativelyPrime(t: (Int, Int)): Boolean = {
      def euclid(x: Int, y: Int): Int = if (y == 0) x else euclid(y, x % y)
      euclid(t._1, t._2) == 1
    }

    (nats product nats).filter(isRelativelyPrime)
  }

  test("interleave") {
    forAll { (xs: Vector[Int]) =>
      // not a complete test but it'll have to do for now
      val s = Streaming.fromVector(xs)
      val r = (s interleave s).iterator.toVector
      for (i <- scala.Range(0, xs.length)) {
        r(i * 2) shouldBe xs(i)
        r(i * 2 + 1) shouldBe xs(i)
      }
    }
  }

  import scala.util.Try

  val bomb: Streaming[Int] =
    Streaming.defer(scala.sys.error("boom"))

  val dangerous: Streaming[Int] =
    1 %:: 2 %:: 3 %:: bomb

  val veryDangerous: Streaming[Int] =
    1 %:: bomb

  test("lazy uncons") {
    veryDangerous.uncons.map(_._1) shouldBe Some(1)
  }

  def isok[U](body: => U) =
    Try(body).isSuccess shouldBe true

  test("lazy map") {
    isok(bomb.map(_ + 1))
  }

  test("lazy flatMap") {
    isok(bomb.flatMap(n => Streaming(n, n)))
  }

  test("lazy filter") {
    isok(bomb.filter(_ > 10))
  }

  test("lazy foldRight") {
    isok(bomb.foldRight(Now(0))((x, total) => total.map(_ + x)))
  }

  test("lazy peekEmpty") {
    bomb.peekEmpty.isDefined should ===  (false)
  }

  test("lazy ++") {
    isok(bomb ++ bomb)
  }

  test("lazier ++") {
    isok(bomb ++ Always(scala.sys.error("ouch"): Streaming[Int]))
  }

  test("lazy zip") {
    isok(bomb zip dangerous)
    isok(dangerous zip bomb)
  }

  test("lazy zipWithIndex") {
    isok(bomb.zipWithIndex)
  }

  test("lazy izip") {
    isok(bomb izip dangerous)
    isok(dangerous izip bomb)
  }

  test("lazy unzip") {
    val bombBomb: Streaming[(Int, Int)] = bomb.map(n => (n, n))
    isok {
      val t: (Streaming[Int], Streaming[Int]) = bombBomb.unzip
      t
    }
  }

  test("lazy merge") {
    isok(bomb merge bomb)
  }

  test("lazy interleave") {
    isok(bomb interleave bomb)
  }

  test("lazy product") {
    isok(bomb product bomb)
  }

  test("lazy take") {
    isok(bomb.take(10))
    isok(bomb.take(0))
  }

  test("take up to the last valid element"){
    isok(dangerous.take(3).toList)
  }

  test("lazy drop") {
    isok(bomb.drop(10))
    isok(bomb.drop(0))
  }

  test("lazy takeWhile") {
    isok(bomb.takeWhile(_ < 10))
  }

  test("lazy dropWhile") {
    isok(bomb.takeWhile(_ < 10))
  }

  test("lazy tails") {
    isok(bomb.tails)
  }
}
