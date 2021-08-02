package cats.collections

import cats._
import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import cats.collections.compat.Factory

/**
 * Dequeue - A Double Ended Queue
 *
 *          Front         Back
 *
 *  <- uncons   ---------   unsnoc ->
 *      cons -> --------- <- snoc
 *
 * Based on the Bankers Double Ended Queue as described by C. Okasaki
 * in "Purely Functional Data Structures"
 *
 * A queue that allows items to be put onto either the front (cons)
 * or the back (snoc) of the queue in constant time, and constant
 * time access to the element at the very front or the very back of
 * the queue.  Dequeuing an element from either end is constant time
 * when amortized over a number of dequeues.
 *
 * This queue maintains an invariant that whenever there are at least
 * two elements in the queue, neither the front list nor back list
 * are empty.  In order to maintain this invariant, a dequeue from
 * either side which would leave that side empty constructs the
 * resulting queue by taking elements from the opposite side
 */
sealed abstract class Dequeue[+A] {
  def isEmpty: Boolean

  def frontOption: Option[A]
  def backOption: Option[A]

  /**
   * destructure from the front of the queue
   */
  def uncons: Option[(A, Dequeue[A])] = this match {
    case SingletonDequeue(a) => Some((a, EmptyDequeue()))
    case FullDequeue(NonEmptyList(f, Nil), 1, NonEmptyList(x, xx :: xs), bs) => {
      val xsr = NonEmptyList(xx, xs).reverse
      Some((f, FullDequeue(xsr, bs - 1, NonEmptyList(x, Nil), 1)))
    }
    case FullDequeue(NonEmptyList(f, Nil), 1, NonEmptyList(single, Nil), 1) => Some((f, SingletonDequeue(single)))
    case FullDequeue(NonEmptyList(f, ff :: fs), s, back, bs) =>
      Some((f, FullDequeue(NonEmptyList(ff, fs), s - 1, back, bs)))
    case _ => None
  }

  /**
   * destructure from the back of the queue
   */
  def unsnoc: Option[(A, Dequeue[A])] = this match {
    case SingletonDequeue(a) => Some((a, EmptyDequeue()))
    case FullDequeue(NonEmptyList(x, xx :: xs), fs, NonEmptyList(b, Nil), 1) => {
      val xsr = NonEmptyList(xx, xs).reverse
      Some((b, FullDequeue(NonEmptyList(x, List.empty), 1, xsr, fs - 1)))
    }
    case FullDequeue(NonEmptyList(single, Nil), 1, NonEmptyList(b, Nil), 1) => Some((b, SingletonDequeue(single)))
    case FullDequeue(front, fs, NonEmptyList(b, bb :: bs), s) =>
      Some((b, FullDequeue(front, fs, NonEmptyList(bb, bs), s - 1)))
    case _ => None
  }

  /**
   * enqueue to the front of the queue
   */
  def cons[AA >: A](a: AA): Dequeue[AA] = this match {
    case SingletonDequeue(single)         => FullDequeue(NonEmptyList(a, List.empty), 1, NonEmptyList(single, List.empty), 1)
    case FullDequeue(front, fs, back, bs) => FullDequeue(NonEmptyList(a, front.toList), fs + 1, back, bs)
    case _                                => SingletonDequeue(a)
  }

  /**
   * enqueue on to the back of the queue
   */
  def snoc[AA >: A](a: AA): Dequeue[AA] = this match {
    case SingletonDequeue(single)         => FullDequeue(NonEmptyList(single, List.empty), 1, NonEmptyList(a, List.empty), 1)
    case FullDequeue(front, fs, back, bs) => FullDequeue(front, fs, NonEmptyList(a, back.toList), bs + 1)
    case _                                => SingletonDequeue(a)
  }

  /**
   * alias for cons
   */
  def +:[AA >: A](a: AA): Dequeue[AA] = cons(a)

  /**
   * alias for snoc
   */
  def :+[AA >: A](a: AA): Dequeue[AA] = snoc(a)

  def toIterator: Iterator[A] = new Iterator[A] {
    private var pos: Dequeue[A] = Dequeue.this

    override def hasNext: Boolean = !pos.isEmpty

    override def next(): A = pos.uncons match {
      case None => throw new NoSuchElementException()
      case Some((a, rest)) =>
        pos = rest
        a
    }
  }

  def to[Col[_], AA >: A](implicit cbf: Factory[AA, Col[AA]]): Col[AA] = {
    val builder = cbf.newBuilder
    @tailrec def go(cur: Dequeue[A]): Unit = cur.uncons match {
      case Some((a, rest)) =>
        builder += a
        go(rest)
      case _ =>
    }
    go(this)
    builder.result()
  }

  def toList: List[A] = to[List, A]

  /**
   * Append another Dequeue to this dequeue
   */
  def ++[AA >: A](other: Dequeue[AA]): Dequeue[AA] = this match {
    case SingletonDequeue(a) => a +: other
    case FullDequeue(f, fs, b, bs) =>
      other match {
        case SingletonDequeue(a) => this :+ a
        case FullDequeue(of, ofs, ob, obs) =>
          FullDequeue(NonEmptyList(f.head, f.tail ++ (b.reverse.toList) ++ of.toList), fs + bs + ofs, ob, obs)
        case _ => this
      }
    case _ => other
  }

  def foldLeft[B](b: B)(f: (B, A) => B): B = this match {
    case SingletonDequeue(a) => f(b, a)
    case FullDequeue(front, _, back, _) => {
      val frontb = front.tail.foldLeft(f(b, front.head))(f)
      val backb = back.tail.foldRight(Eval.now(frontb))((a, b) => b.map(f(_, a))).value
      f(backb, back.head)
    }
    case _ => b
  }

  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = this match {
    case SingletonDequeue(a) => f(a, b)
    case FullDequeue(front, _, back, _) =>
      front.foldRight(Eval.defer(back.reverse.foldRight(b)(f)))(f)
    case _ => b
  }

  def map[B](f: A => B): Dequeue[B] = {
    this match {
      case SingletonDequeue(a) => SingletonDequeue(f(a))
      case FullDequeue(front, fs, back, bs) => {
        FullDequeue(front.map(f), fs, back.map(f), bs)
      }
      case _ => EmptyDequeue()
    }
  }

  def flatMap[B](f: A => Dequeue[B]): Dequeue[B] = {
    def go(n: NonEmptyList[A]): (ListBuffer[B], Int, Option[B]) = {
      val lb = new ListBuffer[B]
      var s = 0
      var o: Option[B] = None

      n.toList.foreach { a =>
        f(a) match {
          case SingletonDequeue(b) =>
            s = s + 1
            o.toList.foreach { x =>
              val _ = lb += x
            }
            o = Some(b)

          case FullDequeue(f, _, b, _) =>
            f.toList.foreach { b =>
              s = s + 1
              o.toList.foreach { x =>
                val _ = lb += x
              }
              o = Some(b)
            }
            b.reverse.toList.foreach { b =>
              s = s + 1
              o.toList.foreach { x =>
                val _ = lb += x
              }
              o = Some(b)
            }
          case _ =>
        }
      }

      (lb, s, o)
    }

    this match {
      case SingletonDequeue(a) => f(a)
      case FullDequeue(front, _, back, _) =>
        val (fl, fs, fo) = go(front)
        val (bl, bs, bo) = go(back.reverse)

        (fo, bo) match {
          case (None, None) => EmptyDequeue()
          case (Some(a), None) =>
            if (fs == 1)
              SingletonDequeue(a)
            else
              FullDequeue(fl.toList.asInstanceOf[NonEmptyList[B]], fs - 1, NonEmptyList(a, List.empty), 1)
          case (None, Some(a)) =>
            if (bs == 1)
              SingletonDequeue(a)
            else
              FullDequeue(NonEmptyList(a, List.empty), 1, bl.toList.asInstanceOf[NonEmptyList[B]], bs - 1)
          case (Some(f), Some(b)) =>
            fl += f
            bl += b
            FullDequeue(fl.toList.asInstanceOf[NonEmptyList[B]],
                        fs,
                        bl.toList.reverse.asInstanceOf[NonEmptyList[B]],
                        bs
            )
        }
      case _ => EmptyDequeue()
    }
  }

  def coflatMap[B](f: Dequeue[A] => B): Dequeue[B] = {
    def loop(op: Option[(A, Dequeue[A])], last: Dequeue[A], acc: Dequeue[B]): Dequeue[B] =
      op match {
        case None            => acc
        case Some((_, rest)) => loop(rest.uncons, rest, acc :+ f(last))
      }

    loop(this.uncons, this, EmptyDequeue())
  }

  def size: Int = this match {
    case SingletonDequeue(_)       => 1
    case FullDequeue(_, fs, _, bs) => fs + bs
    case _                         => 0
  }

  def reverse: Dequeue[A] = this match {
    case FullDequeue(front, fs, back, bs) => FullDequeue(back, bs, front, fs)
    case x                                => x
  }
}

/**
 * special case of the queue when it contains just a single element
 * which can be accessed from either side of the queue
 */
final private[collections] case class SingletonDequeue[A](single: A) extends Dequeue[A] {
  override def isEmpty: Boolean = false
  override def frontOption: Option[A] = Some(single)
  override def backOption: Option[A] = Some(single)
}

/**
 * a queue which has at least two elements, it is guaranteed that the
 * front list and back lists cannot be empty
 */
final private[collections] case class FullDequeue[A](front: NonEmptyList[A],
                                                     fsize: Int,
                                                     back: NonEmptyList[A],
                                                     backSize: Int
) extends Dequeue[A] {
  override def isEmpty: Boolean = false
  override def frontOption: Option[A] = Some(front.head)
  override def backOption: Option[A] = Some(back.head)
}

/**
 * a queue which has no elements
 */
private[collections] case object EmptyDequeue extends Dequeue[Nothing] { self =>
  override val isEmpty: Boolean = true
  override val frontOption: Option[Nothing] = None
  override val backOption: Option[Nothing] = None

  override def toString: String = "EmptyDequeue"

  def apply[A](): Dequeue[A] = self
  def unapply[A](q: Dequeue[A]): Boolean = q.isEmpty
}

object Dequeue extends DequeueInstances {
  def apply[A](as: A*): Dequeue[A] = as.foldLeft[Dequeue[A]](empty)((q, a) => q :+ a)

  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Dequeue[A] =
    F.foldLeft[A, Dequeue[A]](fa, empty)((q, a) => q :+ a)

  def empty[A]: Dequeue[A] = EmptyDequeue()
}

sealed trait DequeueInstances {
  implicit def dequeueEqual[A](implicit eqA: Eq[A]): Eq[Dequeue[A]] = new Eq[Dequeue[A]] {
    final override def eqv(a: Dequeue[A], b: Dequeue[A]): Boolean =
      iteratorEq(a.toIterator, b.toIterator)
  }

  implicit def dequeueMonoid[A]: Monoid[Dequeue[A]] = new Monoid[Dequeue[A]] {
    override val empty: Dequeue[A] = Dequeue.empty
    override def combine(l: Dequeue[A], r: Dequeue[A]) = l ++ r
  }

  implicit val dequeueInstance: Traverse[Dequeue] with MonoidK[Dequeue] with CoflatMap[Dequeue] = new Traverse[Dequeue]
    with MonoidK[Dequeue]
    with CoflatMap[Dequeue] {
    override def empty[A]: Dequeue[A] = Dequeue.empty

    override def combineK[A](l: Dequeue[A], r: Dequeue[A]): Dequeue[A] = l ++ r

    override def map[A, B](fa: Dequeue[A])(f: A => B) = fa.map(f)

    override def coflatMap[A, B](fa: Dequeue[A])(f: Dequeue[A] => B): Dequeue[B] = fa.coflatMap(f)

    override def foldLeft[A, B](fa: Dequeue[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: Dequeue[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

    override def traverse[G[_], A, B](fa: Dequeue[A])(f: A => G[B])(implicit G: Applicative[G]): G[Dequeue[B]] = {
      val gba = G.pure(EmptyDequeue[B]())
      fa.foldLeft(gba)((bs, a) => G.map2(bs, f(a))(_ :+ _))
    }
  }
}
