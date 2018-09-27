package cats.collections

import scala.annotation.tailrec
import cats._, cats.Eval._
import cats.implicits._

/**
 * `Streaming[A]` represents a stream of values. A stream can be
 * thought of as a collection, with two key differences:
 *
 *  1. It may be infinite; it does not necessarily have a finite
 *     length. For this reason, there is no `.length` method.
 *
 *  2. It may be lazy. In other words, the entire stream may not be in
 *     memory. In this case, each "step" of the stream has
 *     instructions for producing the next step.
 *
 * Streams are not necessarily lazy: they use `Eval[Streaming[A]]` to
 * represent a tail that may (or may not be) lazy. If `now[A]` is used
 * for each tail, then `Streaming[A]` will behave similarly to
 * `List[A]`. If `Later[A]` is used for each tail, then `Streaming[A]`
 * will behave similarly to `scala.Stream[A]` (i.e. it will
 * lazily-compute the tail, and will memoize the result to improve the
 * performance of repeated traversals). If `always[A]` is used for
 * each tail, the result will be a lazy stream which does not memoize
 * results (saving space at the cost of potentially-repeated
 * calculations).
 *
 * Since `Streaming[A]` has been compared to `scala.Stream[A]` it is
 * worth noting some key differences between the two types:
 *
 *  1. When the entire stream is known ahead of time, `Streaming[A]`
 *     can represent it more efficiently, using `now[A]`, rather than
 *     allocating a list of closures.
 *
 *  2. `Streaming[A]` does not memoize by default. This protects
 *     against cases where a reference to head will prevent the entire
 *     stream from being garbage collected, and is a better default.
 *     A stream can be memoized later using the `.memoize` method.
 *
 *  3. `Streaming[A]` does not inherit from the standard collections,
 *     meaning a wide variety of methods which are dangerous on
 *     streams (`.length`, `.apply`, etc.) are not present.
 *
 *  4. `scala.Stream[A]` requires an immediate value for `.head`. This
 *     means that operations like `.filter` will block until a
 *     matching value is found, or the stream is exhausted (which
 *     could be never in the case of an infinite stream). By contrast,
 *     `Streaming[A]` values can be totally lazy (and can be
 *     lazily-constructed using `Streaming.defer()`), so methods like
 *     `.filter` are completely lazy.
 *
 *  5. The use of `Eval[Streaming[A]]` to represent the "tail" of the
 *     stream means that streams can be lazily (and safely)
 *     constructed with `Foldable#foldRight`, and that `.map` and
 *     `.flatMap` operations over the tail will be safely trampolined.
 */
@deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
sealed abstract class Streaming[A] extends Product with Serializable { lhs =>
  import Streaming.{Empty, Wait, Cons}

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted. The head will be
   * evaluated, whereas the tail will remain (potentially) lazy within
   * Eval.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def uncons: Option[(A, Eval[Streaming[A]])] = {
    @tailrec def unroll(s: Streaming[A]): Option[(A, Eval[Streaming[A]])] =
      s match {
        case Empty() => None
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => Some((a, lt))
      }
    unroll(this)
  }

  /**
   * Lazily transform the stream given a function `f`.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def map[B](f: A => B): Streaming[B] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.map(f)))
      case Cons(a, lt) => Cons(f(a), lt.map(_.map(f)))
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def unroll(s: Streaming[A], b: B): B =
      s match {
        case Empty() => b
        case Wait(lt) => unroll(lt.value, b)
        case Cons(a, lt) => unroll(lt.value, f(b, a))
      }
    unroll(this, b)
  }

  /**
   * Lazily fold the stream to a single value from the right.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case Empty() => b
      case Wait(lt) => lt.flatMap(_.foldRight(b)(f))
      case Cons(a, lt) => f(a, lt.flatMap(_.foldRight(b)(f)))
    }

  /**
   * Lazily concatenate two streams.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def ++(rhs: Streaming[A]): Streaming[A] =
    this match {
      case Empty() => rhs
      case Wait(lt) => Wait(lt.map(_ ++ rhs))
      case Cons(a, lt) => Cons(a, lt.map(_ ++ rhs))
    }

  /**
   * Lazily concatenate two streams.
   *
   * In this case the evaluation of the second stream may be deferred.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def ++(rhs: Eval[Streaming[A]]): Streaming[A] =
    this match {
      case Empty() => Wait(rhs)
      case Wait(lt) => Wait(lt.map(_ ++ rhs))
      case Cons(a, lt) => Cons(a, lt.map(_ ++ rhs))
    }

  /**
   * Lazily zip two streams together, using the given function `f` to
   * produce output values.
   *
   * The length of the result will be the shorter of the two
   * arguments.
   *
   * The expression:
   *
   *   (lhs zipMap rhs)(f)
   *
   * is equivalent to (but more efficient than):
   *
   *   (lhs zip rhs).map { case (a, b) => f(a, b) }
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def zipMap[B, C](rhs: Streaming[B])(f: (A, B) => C): Streaming[C] =
    (lhs, rhs) match {
      case (Cons(a, lta), Cons(b, ltb)) =>
        Cons(f(a, b), for { ta <- lta; tb <- ltb } yield (ta zipMap tb)(f))
      case (Empty(), _) =>
        Empty()
      case (_, Empty()) =>
        Empty()
      case (Wait(lta), s) =>
        Wait(lta.map(_.zipMap(s)(f)))
      case (s, Wait(ltb)) =>
        Wait(ltb.map(s.zipMap(_)(f)))
    }

  /**
   * Zip two streams together, using the given function `f` to produce
   * the output values.
   *
   * Unlike zipMap, the length of the result will be the *longer* of
   * the two input streams. The functions `g` and `h` will be used in
   * this case to produce valid `C` values.
   *
   * The expression:
   *
   *   (lhs izipMap rhs)(f, g, h)
   *
   * is equivalent to (but more efficient than):
   *
   *   (lhs izip rhs).map {
   *     case Ior.Both(a, b) => f(a, b)
   *     case Ior.Left(a) => g(a)
   *     case Ior.Right(b) => h(b)
   *   }
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def izipMap[B, C](rhs: Streaming[B])(f: (A, B) => C, g: A => C, h: B => C): Streaming[C] =
    (lhs, rhs) match {
      case (Cons(a, lta), Cons(b, ltb)) =>
        Cons(f(a, b), for { ta <- lta; tb <- ltb } yield (ta izipMap tb)(f, g, h))
      case (Wait(lta), tb) =>
        Wait(lta.map(_.izipMap(tb)(f, g, h)))
      case (ta, Wait(ltb)) =>
        Wait(ltb.map(ta.izipMap(_)(f, g, h)))
      case (Empty(), tb) =>
        tb.map(h)
      case (ta, Empty()) =>
        ta.map(g)
    }

  /**
   * Return true if every element of the stream satisfies the
   * predicate, false otherwise.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def forall(f: A => Boolean): Boolean = {
    @tailrec def unroll(s: Streaming[A]): Boolean =
      s match {
        case Empty() => true
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => if (f(a)) unroll(lt.value) else false
      }
    unroll(this)
  }

  /**
   * Provide a list of elements in the stream.
   *
   * This will evaluate the stream immediately, and will hang in the
   * case of infinite streams.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def toList: List[A] = foldLeft[List[A]](List.empty)((as,a) => a :: as).reverse

}

@deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
object Streaming extends StreamingInstances {

  /**
   * Concrete Streaming[A] types:
   *
   *  - Empty(): an empty stream.
   *  - Cons(a, tail): a non-empty stream containing (at least) `a`.
   *  - Wait(tail): a deferred stream.
   *
   * Cons represents a lazy, possibly infinite stream of values.
   * Eval[_] is used to represent possible laziness (via now, later,
   * and always). The head of `Cons` is eager -- a lazy head can be
   * represented using `Wait(always(...))` or `Wait(Later(...))`.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  final case class Empty[A]() extends Streaming[A]
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  final case class Wait[A](next: Eval[Streaming[A]]) extends Streaming[A]
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  final case class Cons[A](a: A, tail: Eval[Streaming[A]]) extends Streaming[A]

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def unfold[A,B](b: B)(f: B => Option[(A,B)]): Streaming[A] = f(b) match {
    case None   => Streaming.empty
    case Some((a,b)) => Streaming.cons(a, defer(unfold(b)(f)))
  }

  /**
   * Create an empty stream of type A.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def empty[A]: Streaming[A] =
    Empty()

  /**
   * Create a stream consisting of a single value.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def apply[A](a: A): Streaming[A] =
    Cons(a, now(Empty()))

  /**
   * Prepend a value to a stream.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def cons[A](a: A, s: Streaming[A]): Streaming[A] =
    Cons(a, now(s))

  /**
   * Prepend a value to an Eval[Streaming[A]].
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def cons[A](a: A, ls: Eval[Streaming[A]]): Streaming[A] =
    Cons(a, ls)

  /**
   * Defer stream creation.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def defer[A](s: => Streaming[A]): Streaming[A] =
    wait(always(s))

  /**
   * Create a stream from an `Eval[Streaming[A]]` value.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def wait[A](ls: Eval[Streaming[A]]): Streaming[A] =
    Wait(ls)

  /**
   * Create a stream from an iterator.
   *
   * The stream will be created lazily, to support potentially large
   * (or infinite) iterators. Iterators passed to this method should
   * not be used elsewhere -- doing so will result in problems.
   *
   * The use case for this method is code like .fromIterable, which
   * creates an iterator for the express purpose of calling this
   * method.
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def fromIteratorUnsafe[A](it: scala.collection.Iterator[A]): Streaming[A] =
    if (it.hasNext) Cons(it.next, Later(fromIteratorUnsafe(it))) else Empty()

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) represents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def unfold[A](o: Option[A])(f: A => Option[A]): Streaming[A] =
    o match {
      case None => Empty()
      case Some(a) => Cons(a, always(unfold(f(a))(f)))
    }
}

private[collections] sealed trait StreamingInstances {
  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  implicit def streamEq[A: Eq]: Eq[Streaming[A]] =
    new Eq[Streaming[A]] {
      def eqv(x: Streaming[A], y: Streaming[A]): Boolean =
        (x izipMap y)(_ === _, _ => false, _ => false)
          .forall(identity)
    }
}
