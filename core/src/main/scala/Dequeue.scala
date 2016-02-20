package dogs

import Predef._
import scala.{annotation,unchecked}

/**
  * A Double-ended queue, based on the Bankers Double Ended Queue as
  * described by C. Okasaki in "Purely Functional Data Structures"
  *
  * A queue that allows items to be put onto either the front (cons)
  * or the back (snoc) of the queue in constant time, and constant
  * time access to the element at the very front or the very back of
  * the queue.  Dequeueing an element from either end is constant time
  * when amortized over a number of dequeues.
  *
  * This queue maintains an invariant that whenever there are at least
  * two elements in the queue, neither the front list nor back list
  * are empty.  In order to maintain this invariant, a dequeue from
  * either side which would leave that side empty constructs the
  * resulting queue by taking elements from the opposite side
  */
sealed abstract class Dequeue[A] {
  import Option._

  def isEmpty: Boolean

  def frontOption: Option[A]
  def backOption: Option[A]

  /**
    * dequeue from the front of the queue
    */
  def uncons: Option[(A, Dequeue[A])] = this match {
    case EmptyDequeue() => Option.none
    case SingletonDequeue(a) => Some((a, EmptyDequeue()))
    case FullDequeue(Nel(f, El()), 1, Nel(x, Nel(xx, xs)), bs) => {
      val xsr = Nel(xx, xs).reverse
      Some((f, FullDequeue(xsr, bs-1, Nel(x, List.empty), 1)))
    }
    case FullDequeue(Nel(f, El()), 1, Nel(single, El()), 1) => Some((f, SingletonDequeue(single)))
    case FullDequeue(Nel(f, Nel(ff, fs)), s, back, bs) => Some((f, FullDequeue(Nel(ff, fs), s-1, back, bs)))
  }

  /**
    * dequeue from the back of the queue
    */
  def unsnoc: Option[(A, Dequeue[A])] = this match {
    case EmptyDequeue() => Option.none
    case SingletonDequeue(a) => Some((a, EmptyDequeue()))
    case FullDequeue(Nel(x, Nel(xx,xs)), fs, Nel(b, El()), 1) => {
      val xsr = Nel(xx, xs).reverse
      Some((b, FullDequeue(Nel(x, List.empty), 1, xsr, fs-1)))
    }
    case FullDequeue(Nel(single, El()), 1, Nel(b, El()), 1) => Some((b, SingletonDequeue(single)))
    case FullDequeue(front, fs, Nel(b, Nel(bb,bs)), s) => Some((b, FullDequeue(front, fs, Nel(bb,bs), s-1)))
  }

  /**
    * enqueue to the front of the queue
    */
  def cons(a: A): Dequeue[A] = this match {
    case EmptyDequeue() => SingletonDequeue(a)
    case SingletonDequeue(single) => FullDequeue(Nel(a, List.empty), 1, Nel(single, List.empty), 1 )
    case FullDequeue(front, fs, back, bs) => FullDequeue(Nel(a, Nel(front.head, front.tail)), fs+1, back, bs)
  }

  /**
    * enqueue on to the back of the queue
    */
  def snoc(a: A): Dequeue[A] = this match {
    case EmptyDequeue() => SingletonDequeue(a)
    case SingletonDequeue(single) => FullDequeue(Nel(single, List.empty), 1, Nel(a, List.empty), 1 )
    case FullDequeue(front, fs, back, bs) => FullDequeue(front, fs, Nel(a, Nel(back.head, back.tail)), bs+1)
  }

  /**
    * alias for cons
    */
  def +:(a: A): Dequeue[A] = cons(a)

  /**
    * alias for snoc
    */
  def :+(a: A): Dequeue[A] = snoc(a)

  /**
    * convert this queue to a stream of elements from front to back
    */
  def toStreaming: Streaming[A] = Streaming.unfold(this)(_.uncons)

  /**
    * convert this queue to a stream of elements from back to front
    */
  def toBackStream: Streaming[A] = Streaming.unfold(this)(_.unsnoc)

  /**
    * Append another Deuque to this dequeue
    */
  def ++(other: Dequeue[A]): Dequeue[A] = this match {
    case EmptyDequeue() => other
    case SingletonDequeue(a) => a +: other
    case FullDequeue(f,fs,b,bs) => other match {
      case EmptyDequeue() => this
      case SingletonDequeue(a) => this :+ a
      case FullDequeue(of,ofs,ob,obs) =>
        FullDequeue(
          Nel(f.head, f.tail ::: (Nel(b.head, b.tail).reverse) ::: of),
          fs + bs + ofs,
          ob,
          obs)
    }
  }

  def foldLeft[B](b: B)(f: (B,A) => B): B = this match {
    case EmptyDequeue() => b
    case SingletonDequeue(a) => f(b, a)
    case FullDequeue(front,_,back,_) => {
      val frontb = front.tail.foldLeft(f(b,front.head))(f)
      val backb = back.tail.foldRight(Eval.now(frontb))((a, b) => b.map(f(_,a))).value
      f(backb,back.head)
    }
  }

  def foldr[B](b: Eval[B])(f: (A,Eval[B]) => Eval[B]): Eval[B] = this match {
    case EmptyDequeue() => b
    case SingletonDequeue(a) => f(a, b)
    case FullDequeue(front,_,back,_) => {
      val backb = back.tail.foldLeft(f(back.head, b))((b,a) => f(a,b))
      val frontb = front.tail.foldRight(backb)(f)
      f(front.head, frontb)
    }
  }

  def foldRight[B](b: B)(f: (A,B) => B): B = this match {
    case EmptyDequeue() => b
    case SingletonDequeue(a) => f(a, b)
    case FullDequeue(front,_,back,_) => {
      val backb = back.tail.foldLeft(f(back.head, b))((b,a) => f(a,b))
      val frontb = front.tail.foldRight(backb)(f)
      f(front.head, frontb)
    }
  }


  def map[B](f: A => B): Dequeue[B] = {
    this match {
      case EmptyDequeue() => EmptyDequeue()
      case SingletonDequeue(a) => SingletonDequeue(f(a))
      case FullDequeue(front, fs, back, bs) => {
        FullDequeue(front.map(f), fs, back.map(f), bs)
      }
    }
  }

  def size: Int = this match {
    case EmptyDequeue() => 0
    case SingletonDequeue(_) => 1
    case FullDequeue(_, fs, _, bs) => fs + bs
  }

  def reverse: Dequeue[A] = this match {
    case FullDequeue(front, fs, back, bs) => FullDequeue(back, bs, front, fs)
    case x => x
  }
}

object Dequeue {
  def apply[A](as: A*) = as.foldLeft[Dequeue[A]](empty)((q,a) ⇒ q :+ a)

/*
  def fromFoldable[F[_],A](fa: F[A])(implicit F: Foldable[F]): Dequeue[A] =
    F.foldLeft[A,Dequeue[A]](fa,empty)((q,a) ⇒ q :+ a)
*/
  def empty[A]: Dequeue[A] = EmptyDequeue()
}

/**
  * special case of the queue when it contains just a single element
  * which can be accessed from either side of the queue
  */
private[dogs] final case class SingletonDequeue[A](single: A) extends Dequeue[A] {
  override def isEmpty = false
  override def frontOption = Option.some(single)
  override def backOption = Option.some(single)
}

/**
  * a queue which has at least two elements, it is guaranteed that the
  * front list and back lists cannot be empty
  */
private[dogs] final case class FullDequeue[A](front: Nel[A], fsize: Int, back: Nel[A], backSize: Int) extends Dequeue[A]  {
  override def isEmpty = false
  override def frontOption = Option.some(front.head)
  override def backOption = Option.some(back.head)
}
/**
  * a queue which has no elements
  */
private[dogs] case object EmptyDequeue extends Dequeue[Nothing] { self =>
  override val isEmpty = true
  override val frontOption = Option.none
  override val backOption = Option.none
  
  override def toString: String = "EmptyDequeue"

  def apply[A]() = self.asInstanceOf[Dequeue[A]]
  def unapply[A](q: Dequeue[A]) = q.isEmpty
}

private[dogs] trait DequeueEqual[A] extends Eq[Dequeue[A]] {
  implicit def A: Eq[A]

  final override def eqv(a: Dequeue[A], b: Dequeue[A]): Boolean =
    Eq[Streaming[A]].eqv(a.toStreaming, b.toStreaming)
}
