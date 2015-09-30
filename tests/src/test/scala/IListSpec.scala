package dogs

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalatest.{FunSuite, PropSpec, Matchers}
import cats._
import cats.syntax.all._
import cats.std.all._
import cats.data._
import IList._
import scala.{Boolean,Byte,Int,List,None,Option,PartialFunction,Some,Vector}
import scala.Predef._
import java.lang.{Exception,String}

object IListTest extends Properties("IListTest") {
  // we need to provid our own tuple instance until
  // https://github.com/non/algebra/pull/82 is merged
  implicit def eqTuple2[A : Eq, B : Eq]: Eq[(A,B)] = Eq.instance { (l,r) =>
    l._1 === r._1 && l._2 === r._2
  }

  implicit def arbitraryIList[A: Arbitrary]: Arbitrary[IList[A]] = Arbitrary(arbitrary[List[A]].map(IList.fromList _))

  implicit val intBooleanArb: Arbitrary[Int => Boolean] = {
    val intGen = implicitly[Arbitrary[Int]].arbitrary
    Arbitrary(Gen.oneOf(
      Gen.const((_: Int) => true),
      Gen.const((_: Int) => false),
      Gen.choose(2, 5).map(n => (a: Int) => a % n == 0),
      Gen.choose(2, 5).map(n => (a: Int) => a % n != 0),
      intGen.map(n => (_: Int) > n),
      intGen.map(n => (_: Int) < n)
    ))
  }

  property("intersperse then remove odd items is identity") = forAll { (a: IList[Int], b: Int) =>
    val isEven = (_: Int) % 2 == 0
    a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) === a
  }

  property("intersperse vs benchmark") = forAll { (a: IList[Int], b: Int) =>
    def intersperse[A](value: IList[A], a: A): IList[A] = value match {
      case INil() => INil()
      case ICons(x, INil()) => x :: INil()
      case ICons(h, t) => h :: a :: intersperse(t, a)
    }
    a.intersperse(b) === intersperse(a, b)
  }

  property("foldl is foldLeft") = forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    rnge.foldLeft(IList[Int]())(_++_) === F.foldLeft(rnge.toList, IList[Int]())(_++_)
  }

  property("foldr is foldRight") = forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    val il = rnge.foldRight(Eval.now(IList[Int]()))((x,xs) => xs.map(xs => x ++ xs)).value
    val l = F.foldRight(rnge.toList, Eval.now(IList[Int]()))((x,xs) => xs.map(xs => x ++ xs)).value

    il === l
  }

  property("mapAccumLeft") = forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    val l = xs.mapAccumLeft(IList[Int]())((c, a) => (c :+ a, f(a)))
    val r = (xs, xs.map(f))

    l === r
  }

  property("mapAccumRight") = forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    val l = xs.mapAccumRight(IList[Int]())((c, a) => (c :+ a, f(a)))
    val r = (xs.reverse, xs.map(f))

    l === r
  }

  // And some other tests that List doesn't have

  property("catamorphism") = forAll { (ns: IList[Int]) =>
    ns.foldRight(Eval.now(IList.empty[Int]))((x,xs) => xs.map(ICons(x, _))).value === ns
  }

  // Functionality borrowed from List is tested in terms of List. Is this ethical?
  // Should they be collapsed into fewer cases?

  property("++") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList === ns.toList ++ ms.toList
  }

  property("++:") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++: ms).toList === ns.toList ++: ms.toList
  }

  property("+:") = forAll { (n: Int, ns: IList[Int]) =>
    (n +: ns).toList === n +: ns.toList
  }

  property("/:") = forAll { (ns: IList[Int], s: String, f: (String, Int) => String) =>
    (s /: ns)(f) === (s /: ns.toList)(f)
  }

  property(":+") = forAll { (n: Int, ns: IList[Int]) =>
    (ns :+ n).toList === ns.toList :+ n
  }

  property("::") = forAll { (n: Int, ns: IList[Int]) =>
    (n :: ns).toList === n :: ns.toList
  }

  property(":::") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ::: ms).toList === ns.toList ::: ms.toList
  }

  property(":\\") = forAll { (ns: IList[Int], s: String, f: (Int, String) => String) =>
    val ff: (Int, Eval[String]) => Eval[String] = (a,b) => Eval.now(f(a,b.value))

    (ns :\ Eval.now(s))(ff).value == (ns.toList :\ Eval.now(s))(ff).value
  }

  property("concat") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns concat ms).toList === ns.toList ++ ms.toList
  }

  property("collect") = forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collect(pf).toList === ns.toList.collect(pf)
  }

  property("collectFirst") = forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collectFirst(pf) === ns.toList.collectFirst(pf)
  }

  property("concat") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList === ns.toList ++ ms.toList
  }

  property("containsSlice") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.containsSlice(ms) === ns.toList.containsSlice(ms.toList)
  }

  property("count") = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.count(p) === ns.toList.count(p)
  }

  property("drop") = forAll { (ns: IList[Int], n: Int) =>
    ns.drop(n).toList === ns.toList.drop(n)
  }

  property("dropRight") = forAll { (ns: IList[Int], n: Int) =>
    ns.dropRight(n).toList === ns.toList.dropRight(n)
  }

  property("dropRightWhile") = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropRightWhile(p).toList === ns.toList.reverse.dropWhile(p).reverse
  }

  property("dropWhile") = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropWhile(p).toList === ns.toList.dropWhile(p)
  }

  property("endsWith") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.endsWith(ms) === ns.toList.endsWith(ms.toList)
  }

  property("fill") = forAll { (a: Byte, b: Int) =>
    IList.fill(a)(b).toList === List.fill(a)(b)
  }

  property("filter") = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.filter(p).toList === ns.toList.filter(p)
  }

  property("filterNot") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.filterNot(f).toList === ns.toList.filterNot(f)
  }

  property("find") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.find(f) === ns.toList.find(f)
  }

  // flatMap and folds are covered by laws

  property("headOption") = forAll { ns: IList[Int] =>
    ns.headOption === ns.toList.headOption
  }

  property("indexOf") = forAll { (ns: IList[Int], n: Int) =>
    ns.indexOf(n).getOrElse(-1) === ns.toList.indexOf(n)
  }

  property("indexOfSlice") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.indexOfSlice(ms).getOrElse(-1) === ns.toList.indexOfSlice(ms.toList)
  }

  property("indexWhere") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.indexWhere(f).getOrElse(-1) === ns.toList.indexWhere(f)
  }

  property("initOption") = forAll { ns: IList[Int] =>
    ns.initOption.map(_.toList) === (try Some(ns.toList.init) catch { case e: Exception => None })
  }

  property("inits") = forAll { ns: IList[Int] =>
    ns.inits.map(_.toList).toList === ns.toList.inits.toList
  }

  // isEmpty is tested by empty laws

  property("lastIndexOf") = forAll { (ns: IList[Int], n: Int) =>
    ns.lastIndexOf(n).getOrElse(-1) === ns.toList.lastIndexOf(n)
  }

  property("lastIndexOfSlice") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.lastIndexOfSlice(ms).getOrElse(-1) === ns.toList.lastIndexOfSlice(ms.toList)
  }

  property("lastIndexWhere") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.lastIndexWhere(f).getOrElse(-1) === ns.toList.lastIndexWhere(f)
  }

  property("lastOption") = forAll { ns: IList[Int] =>
    ns.lastOption === ns.toList.lastOption
  }

  property("length") = forAll { ns: IList[Int] =>
    ns.length === ns.toList.length
  }

  // map is tested by functor laws

  property("nonEmpty") = forAll { ns: IList[Int] =>
    ns.nonEmpty === ns.toList.nonEmpty
  }

  property("padTo") = forAll { (ns: IList[Int], n: Int) =>
    ns.padTo(100, n).toList === ns.toList.padTo(100, n)
  }

  property("patch") = forAll { (ns: IList[Int], a: Int, ms: IList[Int], b: Int) =>
    ns.patch(a, ms, b).toList === ns.toList.patch(a, ms.toList, b)
  }

  property("prefixLength") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.prefixLength(f) === ns.toList.prefixLength(f)
  }

  property("reduceLeftOption") = forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceLeftOption(f) === (try Some(ns.toList.reduceLeft(f)) catch { case e:Exception => None })
  }

  property("reduceRightOption") = forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceRightOption(f) === (try Some(ns.toList.reduceRight(f)) catch { case e:Exception => None })
  }

  property("prefixLength") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.prefixLength(f) === ns.toList.prefixLength(f)
  }

  property("reverse") = forAll { ns: IList[Int] =>
    ns.reverse.toList === ns.toList.reverse
  }

  property("reverseMap") = forAll { (ns: IList[Int], f: Int => Int) =>
    ns.reverseMap(f).toList === ns.toList.reverseMap(f)
  }

  property("reverse_:::") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns reverse_::: ms).toList === (ns.toList reverse_::: ms.toList)
  }

  property("scanLeft") = forAll { (ss: IList[String], f: (Int, String) => Int) =>
    ss.scanLeft(0)(f).toList === ss.toList.scanLeft(0)(f) &&
    ss.scanLeft("z")(_ + _).toList === ss.toList.scanLeft("z")(_ + _) &&
    ss.scanLeft(IList.empty[String])(_ :+ _).toList === ss.toList.scanLeft(IList.empty[String])(_ :+ _)
  }

  property("scanRight") = forAll { (ss: IList[String], f: (String, Int) => Int)  =>
    ss.scanRight(0)(f).toList === ss.toList.scanRight(0)(f) &&
    ss.scanRight("z")(_ + _).toList === ss.toList.scanRight("z")(_ + _) &&
    ss.scanRight(IList.empty[String])(_ +: _).toList === ss.toList.scanRight(IList.empty[String])(_ +: _)
  }

  property("slice") = forAll { (ns: IList[Int], a: Int, b: Int) =>
    ns.slice(a, b).toList === ns.toList.slice(a, b)
  }

  property("sortBy") = forAll { (ss: IList[String], f: String => Int) =>
    ss.sortBy(f).toList === ss.toList.sortBy(f)
  }

  property("sorted") = forAll { (ss: IList[String]) =>
    ss.sorted.toList === ss.toList.sorted
  }

  property("span") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    val (a,b) = ns.span(f)
    (a.toList, b.toList) === ns.toList.span(f)
  }

  property("splitAt") = forAll { (ns: IList[Int], n: Int) =>
    val (a,b) = ns.splitAt(n)
    (a.toList, b.toList) === ns.toList.splitAt(n)
  }

  property("startsWith") = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.startsWith(ms) === ns.toList.startsWith(ms.toList)
  }

  property("tails") = forAll { ns: IList[Int] =>
    ns.tails.map(_.toList).toList === ns.toList.tails.toList
  }

  property("tailOption") = forAll { ns: IList[Int] =>
    ns.tailOption.map(_.toList) === (try Some(ns.toList.tail) catch { case e: Exception => None })
  }

  property("take") = forAll { (ns: IList[Int], n: Byte) =>
    ns.take(n).toList === ns.toList.take(n)
  }

  property("takeRight") = forAll { (ns: IList[Int], n: Byte) =>
    ns.takeRight(n).toList === ns.toList.takeRight(n)
  }

  property("takeRightWhile") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeRightWhile(f).toList === ns.toList.reverse.takeWhile(f).reverse
  }

  property("takeWhile") = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeWhile(f).toList === ns.toList.takeWhile(f)
  }

  property("toStreaming") = forAll { ns: List[Int] =>
    IList(ns: _*).toStreaming.toList === Streaming.fromList(ns).toList
  }

  property("toList") = forAll { ns: List[Int] =>
    IList(ns: _*).toList === ns
  }

  property("toMap") = forAll { ps: List[(String, Int)] =>
    IList(ps: _*).toMap[String,Int] === ps.toMap
  }

  property("toNel") = forAll { ns: List[Int] =>
    IList(ns: _*).toNel === ns.headOption.map(h => OneAnd[Int,List](h, ns.tail))
  }

  property("toStream") = forAll { ns: List[Int] =>
    IList(ns: _*).toStream === ns.toStream
  }

  property("toVector") = forAll { ns: Vector[Int] =>
    IList(ns: _*).toVector === ns
  }

  // uncons is tested everywhere

  property("updated") = forAll { (ns: IList[Int], i: Int, n: Int) =>
    if (i < 0 || i >= ns.length) {
      ns.updated(i, n) === ns
    } else {
      ns.updated(i, n).toList === ns.toList.updated(i, n)
    }
  }

  property("unzip") = forAll { (ns: IList[(Int, String)]) =>
    val (i,s) = ns.unzip[Int,String]
    (i.toList, s.toList) === ns.toList.unzip
  }

  // widen is tested by toMap and unzip
  // zip is tested by zip laws

  property("zipWithIndex") = forAll { ns: IList[Int] =>
    ns.zipWithIndex.toList === ns.toList.zipWithIndex
  }
}
