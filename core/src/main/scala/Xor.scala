package dogs

import Predef._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.Throwable
import scala.Null

/** Represents a right-biased disjunction that is either an `A` or a `B`.
 *
 * An instance of `A [[Xor]] B` is either a `[[Xor.Left Left]][A]` or
 * a `[[Xor.Right Right]][B]`.
 *
 * A common use of [[Xor]] is to explicitly represent the possibility
 * of failure in a result as opposed to throwing an exception.  By
 * convention, [[Xor.Left Left]] is used for errors and [[Xor.Right
 * Right]] is reserved for successes.
 * 
 * For example, a function that attempts to parse an integer from a
 * string may have a return type of `NumberFormatException [[Xor]]
 * Int`. However, since there is no need to actually throw an
 * exception, the type (`A`) chosen for the "left" could be any type
 * representing an error and has no need to actually extend
 * `Exception`.
 *
 * `A [[Xor]] B` is isomorphic to `scala.Either[A, B]`, but [[Xor]] is
 * right-biased, so methods such as `map` and `flatMap` apply only in
 * the context of the "right" case. This right bias makes [[Xor]] more
 * convenient to use than `scala.Either` in a monadic context. Methods
 * such as `swap`, and `leftMap` provide functionality that
 * `scala.Either` exposes through left projections.
 */
sealed abstract class Xor[+A, +B] extends Product with Serializable {
  import Option._
  import Xor._

  /**
   * The catamorphism for Xor
   */
  def fold[C](fa: A => C, fb: B => C): C = this match {
    case Xor.Left(a) => fa(a)
    case Xor.Right(b) => fb(b)
  }

  /**
   * Return a Left if the value is on the Right .
   * Return a Right if the value is on the Left.
   */
  def swap: B Xor A = this match {
    case Right(a) => Left(a)
    case Left(a) => Right(a)
  }

  def isLeft: Boolean = fold(_ => true, _ => false)
  def isRight: Boolean = fold(_ => false, _ => true)

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: B => Unit): Unit = fold(_ => (), f)

  /**
   * Return the Right value, or the default if Left
   * 
   * {{{
   * scala> import dogs._, Predef._
   * scala> Xor.left(1).getOrElse(2)
   * res0: Int = 2
   * scala> Xor.right(1).getOrElse(2)
   * res0: Int = 1
   * }}}
   */
  def getOrElse[BB >: B](default: => BB): BB = fold(_ => default, identity)

  /**
   * Return this if it is Valid, or else fall back to the given default.
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> right[String,Int](1) orElse right[String,Int](2)
   * res0: Xor[String,Int] = Right(1)
   * scala> left[String,Int]("error") orElse right[String,Int](2)
   * res1: Xor[String,Int] = Right(2)
   * scala> left[String,Int]("error") orElse left[String,Int]("also error")
   * res2: Xor[String,Int] = Left(also error)
   * }}}
   */
  def orElse[C, BB >: B](fallback: => C Xor BB): C Xor BB = this match {
    case Xor.Left(_)      => fallback
    case r @ Xor.Right(_) => r
  }

  /**
   * Is this Valid and matching the given predicate
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> left[String,Int]("invalid").exists(_ => true)
   * res0: Boolean = false
   * scala> right[Unit,String]("asdf").exists(_.startsWith("q"))
   * res1: Boolean = false
   * scala> right[Unit,String]("qwer").exists(_.startsWith("q"))
   * res2: Boolean = true
   * }}}
   */
  def exists(f: B => Boolean): Boolean = fold(_ => false, f)


  def recover[BB >: B](pf: PartialFunction[A, BB]): A Xor BB = this match {
    case Xor.Left(a) if pf.isDefinedAt(a) => Xor.right(pf(a))
    case _                                => this
  }

  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, AA Xor BB]): AA Xor BB = this match {
    case Xor.Left(a) if pf.isDefinedAt(a) => pf(a)
    case _                                => this
  }

  def valueOr[BB >: B](f: A => BB): BB = fold(f, identity)

  /**
   * Is this Invalid or matching the predicate
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> left[String,Int]("error").forall(_ % 2 == 0)
   * res0: Boolean = true
   * scala> right[String,Int](1).forall(_ % 2 == 0)
   * res1: Boolean = false
   * scala> right[String,Int](2).forall(_ % 2 == 0)
   * res2: Boolean = true
   * }}}
   */
  def forall(f: B => Boolean): Boolean = fold(_ => true, f)


  def ensure[AA >: A](ifLeft: => AA)(f: B => Boolean): AA Xor B =
    fold(_ => this, b => if (f(b)) this else Xor.Left(ifLeft))

  def toIor: A Ior B = fold(Ior.left, Ior.right)

  def toStdEither: scala.Either[A, B] = fold(scala.Left(_), scala.Right(_))

  /**
   * Returns Right values wrapped in Some, and None for Left values
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> right(1).toOption
   * res0: Option[Int] = Some(1)
   * scala> left[String,Int]("hi").toOption
   * res1: Option[Int] = None
   * }}}
   */
  def toOption[BB >: B]: Option[BB] = fold(_ => None(), Some(_))

<<<<<<< HEAD
  /**
   * Convert this value to a single element List if it is Right,
   * otherwise return an empty List
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> right[Stirng,Int](1).toList
   * res0: List(1)
   * scala> left[String,Int]("error").toList
   * res1: End
   * }}}
   */
=======
>>>>>>> added some benchmarks for list.
  def toList[BB >: B]: List[BB] = fold(_ => List.empty, _ :: List.empty)

  /**
   * Convert this value to Valid if Right or Invalid if Left
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> left[String,Int]("error").toValidated
   * res0: Validated[String,Int] = Invalid(error)
   * scala> right[String,Int](1).toValidated
   * res1: Validated[String,Int] = Valid(1)
   * }}}
   */
  def toValidated: Validated[A,B] = fold(Validated.Invalid.apply, Validated.Valid.apply)

  /**
   * Convert to a Validated, apply a function, convert back.
   */
  def withValidated[AA,BB](f: Validated[A,B] => Validated[AA,BB]): AA Xor BB =
    f(toValidated).toXor

  /**
   * This method applies one of the given functions.
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> right[String,Int](1).bimap(_.length, _ + 1)
   * res0: Xor[Int,Int] = Right(2)
   * scala> left[String,Int]("hello").bimap(_.length, _ + 1)
   * res1: Xor[Int,Int] = Left(5)
   * }}}
   */
  def bimap[C, D](fa: A => C, fb: B => D): C Xor D = this match {
    case Xor.Left(a) => Xor.Left(fa(a))
    case Xor.Right(b) => Xor.Right(fb(b))
  }

  /**
   * Apply a function to a Right value, returning a new Xor value,
   * An Left value is returned unchanged.
   * 
   * {{{
   * scala> import dogs._, Predef._, Xor._
   * scala> right(1).map(_ + 1)
   * res0: Xor[Nothing, Int] = Right(2)
   * scala> left[String,Int]("error").map(_ + 1)
   * res1: Xor[String,Int] = Left(error)
   * }}}
   */
  def map[D](f: B => D): A Xor D = this match {
    case l @ Xor.Left(_) => l
    case Xor.Right(b)    => Xor.Right(f(b))
  }

  def leftMap[C](f: A => C): C Xor B = this match {
    case Xor.Left(a)      => Xor.Left(f(a))
    case r @ Xor.Right(_) => r
  }

  def flatMap[AA >: A, D](f: B => AA Xor D): AA Xor D = this match {
    case l @ Xor.Left(_) => l
    case Xor.Right(b) => f(b)
  }

  def ===[AA >: A, BB >: B](that: AA Xor BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = fold(
    a => that.fold(AA.eqv(a, _), _ => false),
    b => that.fold(_ => false, BB.eqv(b, _))
  )

  def foldLeft[C](c: C)(f: (C, B) => C): C = fold(_ => c, f(c, _))

  def foldr[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(_ => lc, b => f(b, lc))

  def merge[AA >: A](implicit ev: B <:< AA): AA = fold(identity, ev.apply)

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = fold(
    a => s"Xor.Left(${AA.show(a)})",
    b => s"Xor.Right(${BB.show(b)})"
  )
}

object Xor extends XorFunctions {
  final case class Left[+A](a: A) extends (A Xor Nothing)
  final case class Right[+B](b: B) extends (Nothing Xor B)
}

trait XorFunctions {
  def left[A, B](a: A): A Xor B = Xor.Left(a)

  def right[A, B](b: B): A Xor B = Xor.Right(b)

  /**
   * Evaluates the specified block, catching exceptions of the specified type and returning them on the left side of
   * the resulting `Xor`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> import dogs._, dogs.Predef._
   * scala> Xor.catchOnly[java.lang.NumberFormatException] { "foo".toInt }
   * res0: Xor[java.lang.NumberFormatException, Int] = Left(java.lang.NumberFormatException: For input string: "foo")
   * }}}
   */
  def catchOnly[T >: Null <: Throwable]: CatchOnlyPartiallyApplied[T] =
    new CatchOnlyPartiallyApplied[T]

  final class CatchOnlyPartiallyApplied[T] private[XorFunctions] {
    def apply[A](f: => A)(implicit CT: ClassTag[T], NT: NotNull[T]): T Xor A =
      try {
        right(f)
      } catch {
        case t if CT.runtimeClass.isInstance(t) =>
          left(t.asInstanceOf[T])
      }
  }

  def catchNonFatal[A](f: => A): Throwable Xor A =
    try {
      right(f)
    } catch {
      case scala.util.control.NonFatal(t) => left(t)
    }

  /**
   * Converts a `Try[A]` to a `Throwable Xor A`.
   */
  def fromTry[A](t: Try[A]): Throwable Xor A =
    t match {
      case Failure(e) => left(e)
      case Success(v) => right(v)
    }

  /**
   * Converts an `Either[A, B]` to an `A Xor B`.
   */
  def fromEither[A, B](e: scala.Either[A, B]): A Xor B =
    e.fold(left, right)

  /**
   * Converts an `Option[B]` to an `A Xor B`, where the provided `ifNone` values is returned on
   * the left of the `Xor` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): A Xor B =
    o.cata(right, left[A, B](ifNone))
}
