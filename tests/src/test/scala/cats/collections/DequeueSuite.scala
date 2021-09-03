package cats.collections

import cats._
import cats.collections.arbitrary.cogen._
import cats.laws.discipline._
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary, _}
import org.scalacheck.Prop._

class DequeueSuite extends DisciplineSuite {
  import Dequeue._

  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  checkAll("Dequeue[Int]", CoflatMapTests[Dequeue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Dequeue]", SerializableTests.serializable(CoflatMap[Dequeue]))

  checkAll("Dequeue[Int] with Option", TraverseTests[Dequeue].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Dequeue]", SerializableTests.serializable(Traverse[Dequeue]))

  @annotation.tailrec
  final def consL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
    case Nil     => q
    case x :: xs => consL(xs, q.cons(x))
  }

  @annotation.tailrec
  final def unconsL[A](q: Dequeue[A], acc: List[A]): List[A] = q.uncons match {
    case None         => acc
    case Some((i, q)) => unconsL(q, i :: acc)
  }

  @annotation.tailrec
  final def snocL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
    case Nil     => q
    case x :: xs => snocL(xs, q.snoc(x))
  }

  @annotation.tailrec
  final def unsnocL[A](q: Dequeue[A], acc: List[A]): List[A] = q.unsnoc match {
    case None         => acc
    case Some((i, q)) => unsnocL(q, i :: acc)
  }

  test("enqueue onto an empty q can be fetched from either end") {
    val x = "xyzzy"
    val q = Dequeue.empty.cons(x)

    assertEquals(q.uncons, Some((x,EmptyDequeue())))
    assertEquals(q.unsnoc, Some((x,EmptyDequeue())))
  }

  property("cons and then uncons")(forAll { (xs: List[Int]) =>
    val q = consL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    assertEquals(xs, l)
  })

  property("snoc and then unsnoc")(forAll { (xs: List[Int]) =>
    val q = snocL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    assertEquals(xs, l)
  })

  property("cons and then unsnoc")(forAll { (xs: List[Int]) =>
    val q = consL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    assertEquals(xs, l.reverse)
  })

  property("snoc and then uncons")(forAll { (xs: List[Int]) =>
    val q = snocL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    assertEquals(xs, l.reverse)
  })

  implicit def genQ[A: Arbitrary]: Arbitrary[Dequeue[A]] = Arbitrary(for {
    l <- getArbitrary[List[A]]
    r <- getArbitrary[List[A]]
  } yield consL(l, snocL(r, Dequeue.empty)))

  property("foldLeft")(forAll{ (q: Dequeue[Int]) =>
    assertEquals(q.foldLeft[List[Int]](List.empty)((xs,x) => x :: xs), q.reverse.toList)
  })

  property("foldRight")(forAll { (q: Dequeue[Int]) =>
    assertEquals(q.foldRight[List[Int]](Eval.now(List.empty))((x,xs) => xs.map(xs => x ::xs)).value, q.toList)
  })

  property("toList")(forAll { (q: Dequeue[Int]) =>
    assertEquals(q.toList, q.toIterator.toList)
  })

  property("toList/reverse")(forAll { (q: Dequeue[Int]) =>
    assertEquals(q.reverse.toList, q.toIterator.toList.reverse)
  })

  property("toList/append")(forAll { (q1: Dequeue[Int], q2: Dequeue[Int]) =>
    assertEquals((q1 ++ q2).toList, q1.toList ::: q2.toList)
  })

  property("toList/Foldable consistency")(forAll { (q: Dequeue[Int]) =>
    assertEquals(q.toList, Foldable[Dequeue].toList(q))
  })

  property("toList/toStream consistency")(forAll { (q: Dequeue[Int]) =>
    assertEquals(q.toList, q.to[Stream, Int].toList)
  })

  property("equality")(forAll { (xs: List[Int]) =>
    val q1 = consL(xs, Dequeue.empty)
    val q2 = snocL(xs.reverse, Dequeue.empty)
    assert(Eq[Dequeue[Int]].eqv(q1, q2), true)
  })

  property("inequality")(forAll { (xs: List[Int], ys: List[Int]) =>
    val q1 = consL(xs, Dequeue.empty)
    val q2 = consL(ys, Dequeue.empty)

    if (xs != ys) {
      assertEquals(Eq[Dequeue[Int]].eqv(q1, q2), false)
    }
  })

}
