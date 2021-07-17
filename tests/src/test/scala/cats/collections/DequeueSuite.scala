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

  checkAll("Dequeue[Int]", CoflatMapTests[Dequeue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Dequeue]", SerializableTests.serializable(CoflatMap[Dequeue]))

  checkAll("Dequeue[Int] with Option", TraverseTests[Dequeue].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Dequeue]", SerializableTests.serializable(Traverse[Dequeue]))

   @annotation.tailrec
   final def consL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case Nil => q
     case x :: xs => consL(xs, q cons x)
   }

   @annotation.tailrec
   final def unconsL[A](q: Dequeue[A], acc: List[A]): List[A] = q.uncons match {
     case None => acc
     case Some((i, q)) => unconsL(q, i :: acc)
   }

   @annotation.tailrec
   final def snocL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case Nil => q
     case x :: xs => snocL(xs, q snoc x)
   }

   @annotation.tailrec
   final def unsnocL[A](q: Dequeue[A], acc: List[A]): List[A] = q.unsnoc match {
     case None => acc
     case Some((i, q)) => unsnocL(q, i :: acc)
   }

  test("enqueue onto an empty q can be fetched from either end"){
    val x = "xyzzy"
    val q = Dequeue.empty cons x

    assertEquals(q.uncons, Some((x,EmptyDequeue())))
    assertEquals(q.unsnoc, Some((x,EmptyDequeue())))
  }

  test("cons and then uncons")(forAll { xs: List[Int] =>
    val q = consL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    assertEquals(xs, l)
  })

  test("snoc and then unsnoc")(forAll { xs: List[Int] =>
    val q = snocL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    assertEquals(xs, l)
  })

  test("cons and then unsnoc")(forAll { xs: List[Int] =>
    val q = consL(xs, Dequeue.empty)
    val l = unsnocL(q, List.empty)

    assertEquals(xs, l.reverse)
  })

  test("snoc and then uncons")(forAll { xs: List[Int] =>
    val q = snocL(xs, Dequeue.empty)
    val l = unconsL(q, List.empty)

    assertEquals(xs, l.reverse)
  })

  implicit def genQ[A: Arbitrary]: Arbitrary[Dequeue[A]] = Arbitrary(
    for {
      l <- getArbitrary[List[A]]
      r <- getArbitrary[List[A]]
    } yield consL(l, snocL(r, Dequeue.empty)))

  test("foldLeft")(forAll{ q: Dequeue[Int] =>
    assertEquals(q.foldLeft[List[Int]](List.empty)((xs,x) => x :: xs), q.reverse.toList)
  })

  test("foldRight")(forAll { q: Dequeue[Int] =>
    assertEquals(q.foldRight[List[Int]](Eval.now(List.empty))((x,xs) => xs.map(xs => x ::xs)).value, q.toList)
  })

  test("toList")(forAll { q: Dequeue[Int] =>
    assertEquals(q.toList, q.toIterator.toList)
  })

  test("toList/reverse")(forAll { q: Dequeue[Int] =>
    assertEquals(q.reverse.toList, q.toIterator.toList.reverse)
  })

  test("toList/append")(forAll { (q1: Dequeue[Int], q2: Dequeue[Int]) =>
    assertEquals((q1 ++ q2).toList, q1.toList ::: q2.toList)
  })

  test("toList/Foldable consistency")(forAll { q: Dequeue[Int] =>
    assertEquals(q.toList, Foldable[Dequeue].toList(q))
  })

  test("toList/toStream consistency")(forAll { q: Dequeue[Int] =>
    assertEquals(q.toList, q.to[Stream, Int].toList)
  })

  test("equality")(forAll { xs: List[Int] =>
    val q1 = consL(xs, Dequeue.empty)
    val q2 = snocL(xs.reverse, Dequeue.empty)
    assert(Eq[Dequeue[Int]].eqv(q1, q2), true)
  })

  test("inequality")(forAll { (xs: List[Int], ys: List[Int]) =>
    val q1 = consL(xs, Dequeue.empty)
    val q2 = consL(ys, Dequeue.empty)
    if (xs != ys) {
      assertEquals(Eq[Dequeue[Int]].eqv(q1, q2), false)
    }
  })

}
