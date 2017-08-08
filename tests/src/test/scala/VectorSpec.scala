package dogs
package tests

import Predef._
import dogs.tests.arbitrary.all._
import cats.implicits._
import cats.laws.discipline._
import cats.kernel.laws.{GroupLaws, OrderLaws}

class VectorTest extends SlowDogsSuite {
  import scala.collection.immutable.{Vector=>SVector, Map=>SMap}
  import dogs.syntax.foldable._
  import Vector._
  import Nel._

  checkAll("Monoid[Vector[Int]]", GroupLaws[Vector[Int]].monoid)
//  checkAll("Vector[Int]", AlternativeTests[Vector].Alternative[Int, Int, Int])
  checkAll("Vector[Int]", CartesianTests[Vector].cartesian[Int, Int, Int])
  checkAll("Vector[Int]", CoflatMapTests[Vector].coflatMap[Int, Int, Int])
  checkAll("Vector[Int]", OrderLaws[Vector[Int]].order)
  checkAll("Vector[Int]", TraverseTests[Vector].traverse[Int,Int,Int,Vector[Int],Option,Option])

/*
  test("intersperse then remove odd items is identity"){
    forAll { (a: Vector[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      (a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) === a) should be(true)
    }
  }
 */
  test("mapAccumLeft") {
    forAll { xs: Vector[dogs.Predef.Int] =>
      val f = (_: dogs.Predef.Int) + 1
      xs.mapAccumLeft(Vector[dogs.Predef.Int](), (c: Vector[dogs.Predef.Int], a) => (c :+ a, f(a))) should ===((xs, xs.map(f)))
    }
  }

  test("mapAccumRight") {
    forAll { xs: Vector[Int] =>
      val f = (_: Int) + 1
      val left: (Vector[Int], Vector[Int]) = xs.mapAccumRight(Vector[Int](), (c: Vector[Int], a) => (c :+ a, f(a)))
      val right: (Vector[Int], Vector[Int]) = (xs.reverse, xs.map(f))
      left should ===(right)
    }
  }


  // And some other tests that Vector doesn't have

  test("catamorphism") {
    forAll {
      (ns: Vector[Int]) =>
      ns.foldLeft(Vector.empty[Int])(_ :+ _) should ===(ns)
    }
  }


  // Functionality borrowed from Vector is tested in terms of Vector. Is this ethical?
  // Should they be collapsed into fewer cases?

  test("++"){
    forAll {
       (ns: Vector[Int], ms: Vector[Int]) =>
      (ns ++ ms).toList should === (ns.toList ++ ms.toList)
    }
  }

  test("+:"){
    forAll {
      (n: Int, ns: Vector[Int]) =>
      (n +: ns).toScalaVector should ===(n +: ns.toScalaVector)
    }
  }

  test("/:"){
    forAll {
       (ns: Vector[Int], s: String, f: (String, Int) => String) =>
      (s /: ns)(f) should ===((s /: ns.toScalaVector)(f))
    }
  }

  test(":+"){
    forAll {
       (n: Int, ns: Vector[Int]) =>
      (ns :+ n).toScalaVector should ===(ns.toScalaVector :+ n)
    }
  }

  test("collect"){
    forAll {
       (ns: Vector[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
      ns.collect(pf).toScalaVector should ===(ns.toScalaVector.collect(pf))
    }
  }

  test("collectFirst"){
    forAll {
       (ns: Vector[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
      ns.collectFirst(pf).toScalaOption should ===(ns.toScalaVector.collectFirst(pf))
    }
  }

  test("concat"){
    forAll {
      (ns: Vector[Int], ms: Vector[Int]) =>
      (ns concat ms).toScalaVector should ===(ns.toScalaVector ++ ms.toScalaVector)
    }
  }

  test("containsSlice"){
    forAll {
       (ns: Vector[Int], ms: Vector[Int]) =>
      ns.containsSlice(ms) should ===(ns.toScalaVector.containsSlice(ms.toScalaVector))
    }
  }

  test("count"){
    forAll {
       (ns: Vector[Int], p: Int => Boolean) =>
      ns.count(p) should ===(ns.toScalaVector.count(p))
    }
  }

  test("drop"){
    forAll {
       (ns: Vector[Int], n: Int) =>
      ns.drop(n).toScalaVector should ===(ns.toScalaVector.drop(n))
    }
  }

  test("dropRight"){
    forAll {
       (ns: Vector[Int], n: Int) =>
      ns.dropRight(n).toScalaVector should ===(ns.toScalaVector.dropRight(n))
    }
  }


  test("dropRightWhile"){
    forAll {
       (ns: Vector[Int], p: Int => Boolean) =>
      ns.dropRightWhile(p).toScalaVector should ===(ns.toScalaVector.reverse.dropWhile(p).reverse)
    }
  }

  test("dropWhile"){
    forAll {
       (ns: Vector[Int], p: Int => Boolean) =>
      ns.dropWhile(p).toScalaVector should ===(ns.toScalaVector.dropWhile(p))
    }
  }

  test("endsWith"){
    forAll {
       (ns: Vector[Int], ms: Vector[Int]) =>
      ns.endsWith(ms) should ===(ns.toScalaVector.endsWith(ms.toScalaVector))
    }
  }

  test("fill"){
    forAll {
       (a: Byte, b: Int) =>
      Vector.fill(a)(b).toScalaVector should ===(SVector.fill(a)(b))
    }
  }

  test("filter"){
    forAll {
      (ns: Vector[Int], p: Int => Boolean) =>
      ns.filter(p).toScalaVector should ===(ns.toScalaVector.filter(p))
    }
  }

  test("filterNot"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.filterNot(f).toScalaVector should ===(ns.toScalaVector.filterNot(f))
    }
  }

  test("find"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.find(f).toScalaOption should ===(ns.toScalaVector.find(f))
    }
  }

  // flatMap and folds are covered by laws

  // test get in terms of foldLeft, optionally modded into vector space
  test("get"){
    forAll {
      (ns: Vector[Int], i: Int, mod: Boolean) =>
      val index = if (mod && ns.length == 0) 0 else if (mod) i % ns.length else i

      val received = ns get index

      val expected:Option[Int] = if (index < 0 || index >= ns.length) {
        None()
      } else {
        val (_, back) = ns.foldLeft((0, None(): Option[Int])) {
          case ((`index`, None()), n) => (0, Some(n))
          case ((_, Some(n)), _) => (0, Some(n))
          case ((i, None()), n) => (i + 1, None())
        }

        back
      }

      received should ===(expected)
    }
  }

  test("groupBy"){
    forAll {
      (ns: Vector[Int], f: Int => Int) =>
      val left: SMap[Int,SVector[Int]] = ns.groupBy(f).map(x => x.toScalaVector).toScalaMap
      val right = ns.toScalaVector.groupBy(f)

      left should ===(right)
    }
  }
/*
  test("groupBy1"){
    forAll {
      (ns: Vector[Int], f: Int => Int) =>
      ns.groupBy1(f).map(x => x.head +: x.tail.toScalaVector).toScalaMap should ===(ns.toScalaVector.groupBy(f))
    }
  }
 */

  test("headOption"){
    forAll {
      ns: Vector[Int] =>
      ns.headOption.toScalaOption should ===(ns.toScalaVector.headOption)
    }
  }

  test("indexOf"){
    forAll {
      (ns: Vector[Int], n: Int) =>
      ns.indexOf(n).getOrElse(-1) should ===(ns.toScalaVector.indexOf(n))
    }
  }

  test("indexOfSlice"){
    forAll {
      (ns: Vector[Int], ms: Vector[Int]) =>
      ns.indexOfSlice(ms).getOrElse(-1) should ===(ns.toScalaVector.indexOfSlice(ms.toScalaVector))
    }
  }

  test("indexWhere"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.indexWhere(f).getOrElse(-1) should ===(ns.toScalaVector.indexWhere(f))
    }
  }

  test("initOption"){
    forAll {
      ns: Vector[Int] =>
      val right = Option.fromScalaOption(scala.Either.catchNonFatal(ns.toScalaVector.init).toOption)
      ns.initOption.map(_.toScalaVector) should ===(right)
    }
  }

  test("inits"){
    forAll {
      ns: Vector[Int] =>
      ns.inits.map(_.toScalaVector).toScalaVector should ===(ns.toScalaVector.inits.toVector)
    }
  }

  // intersperse is tested above
  // isEmpty is tested by empty laws

  test("lastIndexOf"){
    forAll {
      (ns: Vector[Int], n: Int) =>
      ns.lastIndexOf(n).getOrElse(-1) should ===(ns.toScalaVector.lastIndexOf(n))
    }
  }

  test("lastIndexOfSlice"){ 
    forAll {
      (ns: Vector[Int], ms: Vector[Int]) =>
      ns.lastIndexOfSlice(ms).getOrElse(-1) should ===(ns.toScalaVector.lastIndexOfSlice(ms.toScalaVector))
    }
  }

  test("lastIndexWhere"){ 
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.lastIndexWhere(f).getOrElse(-1) should ===(ns.toScalaVector.lastIndexWhere(f))
    }
  }

  test("lastOption"){ 
    forAll {
      ns: Vector[Int] =>
      ns.lastOption.toScalaOption should ===(ns.toScalaVector.lastOption)
    }
  }

  test("length"){
    forAll {
      ns: Vector[Int] =>
      ns.length should ===(ns.toScalaVector.length)
    }
  }

  // map is tested by functor laws

  test("nonEmpty"){ 
    forAll {
      ns: Vector[Int] =>
      ns.nonEmpty should ===(ns.toScalaVector.nonEmpty)
    }
  }

  test("padTo"){ 
    forAll {
      (ns: Vector[Int], n: Int) =>
      ns.padTo(100, n).toScalaVector should ===(ns.toScalaVector.padTo(100, n))
    }
  }

  test("patch"){ 
    forAll {
      (ns: Vector[Int], a: Int, ms: Vector[Int], b: Int) =>
      ns.patch(a, ms, b).toScalaVector should ===(ns.toScalaVector.patch(a, ms.toScalaVector, b))
    }
  }

  test("prefixLength"){ 
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.prefixLength(f) should ===(ns.toScalaVector.prefixLength(f))
    }
  }


  test("reduceLeftOption"){ 
    forAll {
      (ns: Vector[Int], f: (Int, Int) => Int) =>
      ns.reduceLeftOption(f).toScalaOption should ===(ns.toScalaVector.reduceLeftOption(f))
    }
  }

  test("reduceRightOption"){ 
    forAll {
      (ns: Vector[Int], f: (Int, Int) => Int) =>
      ns.reduceRightOption(f).toScalaOption should ===(ns.toScalaVector.reduceRightOption(f))
    }
  }

  test("reverse"){ 
    forAll {
      ns: Vector[Int] =>
      ns.reverse.toScalaVector should  ===(ns.toScalaVector.reverse)
    }
  }

  test("reverseMap"){ 
    forAll {
      (ns: Vector[Int], f: Int => Int) =>
      ns.reverseMap(f).toScalaVector should ===(ns.toScalaVector.reverseMap(f))
    }
  }

  test("scanLeft"){ 
    forAll {
      (ss: Vector[String], f: (Int, String) => Int) =>
      ss.scanLeft(0)(f).toScalaVector should ===(ss.toScalaVector.scanLeft(0)(f))
      ss.scanLeft("z")(_ + _).toScalaVector should ===(ss.toScalaVector.scanLeft("z")(_ + _))
      ss.scanLeft(Vector.empty[String])(_ :+ _).toScalaVector should ===(ss.toScalaVector.scanLeft(Vector.empty[String])(_ :+ _))
    }
  }

  test("scanRight"){
    forAll {
      (ss: Vector[String], f: (String, Int) => Int)  =>
      ss.scanRight(0)(f).toScalaVector should ===(ss.toScalaVector.scanRight(0)(f))
      ss.scanRight("z")(_ + _).toScalaVector should ===(ss.toScalaVector.scanRight("z")(_ + _))
      ss.scanRight(Vector.empty[String])(_ +: _).toScalaVector should ===(ss.toScalaVector.scanRight(Vector.empty[String])(_ +: _))
    }
  }

  test("slice"){
    forAll {
      (ns: Vector[Int], a: Int, b: Int) =>
      ns.slice(a, b).toScalaVector should ===(ns.toScalaVector.slice(a, b))
    }
  }

/*
  test("sortBy"){
    forAll {
      (ss: Vector[String], f: String => Int) =>
      ss.sortBy(f).toScalaVector should ===(ss.toScalaVector.sortBy(f))
    }
  }

  test("sorted"){
    forAll {
      (ss: Vector[String]) =>
      ss.sorted.toScalaVector should ===(ss.toScalaVector.sorted)
    }
  }
 */

  test("span"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      val (x,y) = ns.span(f)
      (x.toScalaVector -> y.toScalaVector) should ===(ns.toScalaVector.span(f))
    }
  }

  test("splitAt"){
    forAll {
      (ns: Vector[Int], n: Int) =>
      val (x,y) = ns.splitAt(n)
                            (x.toScalaVector -> y.toScalaVector) should ===(ns.toScalaVector.splitAt(n))

    }
  }

  test("startsWith"){
    forAll {
      (ns: Vector[Int], ms: Vector[Int]) =>
      ns.startsWith(ms) should ===(ns.toScalaVector.startsWith(ms.toScalaVector))
    }
  }

  test("tails"){
    forAll {
      ns: Vector[Int] =>
      ns.tails.map(_.toScalaVector).toScalaVector should ===(ns.toScalaVector.tails.toVector)
    }
  }

  test("tailOption"){
    forAll {
      ns: Vector[Int] =>
      val sc = Option.fromScalaOption(scala.Either.catchNonFatal(ns.toScalaVector.tail).toOption)
      ns.tailOption.map(_.toScalaVector) should === (sc)
    }
  }

  test("take"){
    forAll {
      (ns: Vector[Int], n: Byte) =>
      ns.take(n).toScalaVector should ===(ns.toScalaVector.take(n))
    }
  }

  test("takeRight"){
    forAll {
      (ns: Vector[Int], n: Byte) =>
      ns.takeRight(n).toScalaVector should ===(ns.toScalaVector.takeRight(n))
    }
  }

  test("takeRightWhile"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.takeRightWhile(f).toScalaVector should ===(ns.toScalaVector.reverse.takeWhile(f).reverse)
    }
  }

  test("takeWhile"){
    forAll {
      (ns: Vector[Int], f: Int => Boolean) =>
      ns.takeWhile(f).toScalaVector should ===(ns.toScalaVector.takeWhile(f))
    }
  }

  test("toMap"){
    forAll {
      ps: Vector[(String, Int)] =>
      ps.toMap[String,Int] should ===(Map(ps.toScalaVector: _*))
    }
  }

  test("toNel"){
    forAll {
      ns: Vector[Int] =>
      ns.toNel should ===(ns.toDogsList.toNel)
    }
  }


  // like some of the other tests, this is terrible and almost vacuous
  test("updated"){
    forAll {
      (ns: Vector[Int], i: Int, n: Int) =>
      if (i < 0 || i >= ns.length) {
        ns.updated(i, n) should ===(ns)
      } else {
        ns.updated(i, n).toScalaVector should ===(ns.toScalaVector.updated(i, n))
      }
    }
  }

  test("unzip"){
    forAll {
      (ns: Vector[(Int, String)]) =>
      val (x,y) = ns.unzip[Int,String]
      (x.toScalaVector, y.toScalaVector) should === (ns.toScalaVector.unzip) 
    }
  }

  // widen is tested by toMap and unzip
  // zip is tested by zip laws



  test("zipWithIndex"){
    forAll {
      ns: Vector[Int] =>
      ns.zipWithIndex.toScalaVector should ===(ns.toScalaVector.zipWithIndex)
    }
  }

}
