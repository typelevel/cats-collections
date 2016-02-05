package dogs
package tests

import Predef._
import dogs.tests.arbitrary._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._
import scala.{annotation}

object DequeueSpec extends Properties("Dequeue") with ArbitraryList {
  import Option._
  import Dequeue._

   @annotation.tailrec
   def consL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case El() => q
     case Nel(x,xs) => consL(xs, q cons x)
   }
 
   @annotation.tailrec
   def unconsL[A](q: Dequeue[A], acc: List[A]): List[A] = q uncons match {
     case None() => acc
     case Some((i, q)) => unconsL(q, i :: acc)
   }
 
   @annotation.tailrec
   def snocL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
     case El() => q
     case Nel(x,xs) => snocL(xs, q snoc x)
   }
 
   @annotation.tailrec
   def unsnocL[A](q: Dequeue[A], acc: List[A]): List[A] = q unsnoc match {
     case None() => acc
     case Some((i, q)) => unsnocL(q, i :: acc)
   }

  property("enqueue onto an empty q can be fetched from either end") = secure {
    val x = "xyzzy"
    val q = empty cons x
    q.uncons == Some((x,EmptyDequeue())) &&
    q.unsnoc == Some((x,EmptyDequeue()))
  }
  property("cons and then uncons") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    val l = unconsL(q, El())

    xs == l
  }

  property("snoc and then unsnoc") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    val l = unsnocL(q, El())

    xs == l
  }

  property("cons and then unsnoc") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    val l = unsnocL(q, El())

    xs == l.reverse
  }

  property("snoc and then uncons") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    val l = unconsL(q, El())

    xs == l.reverse
  }

  implicit def genQ[A: Arbitrary]: Arbitrary[Dequeue[A]] = Arbitrary(
    for {
      l <- getArbitrary[List[A]]
      r <- getArbitrary[List[A]]
    } yield consL(l, snocL(r, empty)))

  property("foldLeft") = forAll { (q: Dequeue[Int]) =>
    q.foldLeft[List[Int]](El())((xs,x) => Nel(x, xs)) == q.toBackStream.toList
  }
  property("foldRight") = forAll { (q: Dequeue[Int]) =>
    q.foldRight[List[Int]](Eval.now(El()))((x,xs) => xs.map(xs => Nel(x,xs))).value == q.toStreaming.toList
  }
}


