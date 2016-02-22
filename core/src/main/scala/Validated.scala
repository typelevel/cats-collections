package dogs

import Predef._
import scala.reflect.ClassTag
import scala.{Product, Serializable, Boolean, Unit, Nothing}
import scala.util.{Failure, Success, Try}

sealed abstract class Validated[+E, +A] extends Product with Serializable {
  import Xor._
  import Validated._
  import Option._

  /**
   * The catamorphism for Validated
   */
  def fold[B](fe: E => B, fa: A => B): B = this match {
      case Invalid(e) => fe(e)
      case Valid(a) => fa(a)
    }

  /**
   * Return a Valid if the value is Invalid .
   * Return an Invalid if the value is Valid.
   */
  def swap: Validated[A, E] = this match {
    case Valid(a) => Invalid(a)
    case Invalid(e) => Valid(e)
  }

  def isValid: Boolean = fold(_ => false, _ => true)
  def isInvalid: Boolean = fold(_ => true, _ => false)

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: A => Unit): Unit = fold(_ => (), f)

  /**
   * Return the Valid value, or the default if Invalid
   * 
   * {{{
   * scala> import dogs._, Predef._
   * scala> Validated.invalid(1).getOrElse(2)
   * res0: Int = 2
   * scala> Validated.valid(1).getOrElse(2)
   * res0: Int = 1
   * }}}
   */
  def getOrElse[B >: A](default: => B): B = fold(_ => default, identity)

  /**
   * Return this if it is Valid, or else fall back to the given default.
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid[String,Int](1) orElse valid[String,Int](2)
   * res0: Validated[String,Int] = Valid(1)
   * scala> invalid[String,Int]("invalid") orElse valid[String,Int](2)
   * res1: Validated[String,Int] = Valid(2)
   * scala> invalid[String,Int]("invalid") orElse invalid[String,Int]("also invalid")
   * res2: Validated[String,Int] = Invalid(also invalid)
   * }}}
   */
  def orElse[EE, AA >: A](default: => Validated[EE,AA]): Validated[EE,AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(_) => default
    }

  /**
   * Is this Valid and matching the given predicate
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> invalid[String,Int]("invalid").exists(_ => true)
   * res0: Boolean = false
   * scala> valid[Unit,String]("asdf").exists(_.startsWith("q"))
   * res1: Boolean = false
   * scala> valid[Unit,String]("qwer").exists(_.startsWith("q"))
   * res2: Boolean = true
   * }}}
   */
  def exists(predicate: A => Boolean): Boolean = fold(_ => false, predicate)

  /**
   * Is this Invalid or matching the predicate
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> invalid[String,Int]("invalid").forall(_ % 2 == 0)
   * res0: Boolean = true
   * scala> valid[String,Int](1).forall(_ % 2 == 0)
   * res1: Boolean = false
   * scala> valid[String,Int](2).forall(_ % 2 == 0)
   * res2: Boolean = true
   * }}}
   */
  def forall(f: A => Boolean): Boolean = fold(_ => true, f)


  /**
   * Converts the value to an Either[E,A]
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid[String,Int](1).toEither
   * res0: Either[String,Int] = Right(1)
   * scala> invalid[String,Int]("hi").toEither
   * res1: Either[String,Int] = Left(hi)
   * }}}
   */
  def toEither: scala.Either[E,A] = fold(scala.Left.apply, scala.Right.apply)

  /**
   * Returns Valid values wrapped in Some, and None for Invalid values
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid(1).toOption
   * res0: Option[Int] = Some(1)
   * scala> invalid[String,Int]("hi").toOption
   * res1: Option[Int] = None
   * }}}
   */
  def toOption[AA >: A]: Option[AA] = fold(_ => None(), Some.apply)

  /**
   * Convert this value to a single element List if it is Valid,
   * otherwise return an empty List
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid[Stirng,Int](1).toList
   * res0: List(1)
   * scala> invalid[String,Int]("invalid").toList
   * res1: End
   * }}}
   */
  def toList[AA >: A]: List[AA] = fold(_ => List.empty, List(_))

  /** 
   * Lift the Invalid value into a NonEmptyList. 
   */
  def toValidatedNel[EE >: E, AA >: A]: ValidatedNel[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e)   => Validated.invalidNel(e)
    }

  /**
   * Convert this value to Right if Valid or Left if Invalid
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> invalid[String,Int]("error").toXor
   * res0: Xor[String,Int] = Left(error)
   * scala> valid[String,Int](1).toXor
   * res1: Xor[String,Int] = Right(1)
   * }}}
   */
  def toXor: Xor[E,A] = fold(Xor.Left.apply,Xor.Right.apply)

  /**
   * Convert to an Xor, apply a function, convert back.
   */
  def withXor[EE,B](f: (E Xor A) => (EE Xor B)): Validated[EE,B] =
    f(toXor).toValidated

  /**
   * This method applies one of the given functions.
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid[String,Int](1).bimap(_.length, _ + 1)
   * res0: Validated[Int,Int] = Valid(2)
   * scala> invalid[String,Int]("hello").bimap(_.length, _ + 1)
   * res1: Validated[Int,Int] = Invalid(5)
   * }}}
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply,
         fa andThen Valid.apply)

  /**
   * Apply a function to a Valid value, returning a new Valid value,
   * An invalid value is returned unchanged.
   * 
   * {{{
   * scala> import dogs._, Predef._, Validated._
   * scala> valid(1).map(_ + 1)
   * res0: Validated[Nothing, Int] = Valid(2)
   * scala> invalid[String,Int]("error").map(_ + 1)
   * res1: Validated[String,Int] = Invalid(error)
   * }}}
   */
  def map[B](f: A => B): Validated[E,B] = bimap(identity, f)

  /**
   * Apply a function to an Invalid value, returning a new Invalid value.
   * Or, if the original valid was Valid, return it.
   */
  def leftMap[EE](f: E => EE): Validated[EE,A] = bimap(f, identity)

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    fold(_ => b, f(b, _))

  /**
   * Lazily-apply the given function to the value with the given B
   * when valid, otherwise return the given B.
   */
  def foldr[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fold(_ => lb, a => f(a, lb))

  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String =
    fold(e => s"Invalid(${EE.show(e)})",
         a => s"Valid(${AA.show(a)})")

  /**
   * Apply a function (that returns a `Validated`) in the valid case.
   * Otherwise return the original `Validated`.
   *
   * This allows "chained" validation: the output of one validation
   * can be fed into another validation function.
   *
   * This function is similar to `Xor.flatMap`. It's not called
   * `flatMap`, because by convention, `flatMap` is a monadic bind
   * that is consistent with `ap`. This method is not consistent with
   * ap (or other `Apply`-based methods), because it has "fail-fast"
   * behavior as opposed to accumulating validation failures.
   */
  def andThen[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a) => f(a)
      case i @ Invalid(_) => i
    }

}

object Validated extends ValidatedFunctions{
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]
}


trait ValidatedFunctions {
  /**
   * Construct a valid value, this is the preferred method for
   * constructing Validated.Valid values, as it coerces the return
   * type to be Validated instead of Valid.
   */
  def valid[A, B](b: B): Validated[A,B] = Validated.Valid(b)

  /**
   * Construct an invalid value, this is the preferred method for
   * constructing Validated.Invalid values, as it coerces the return
   * type to be Validated instead of Invalid.
   */
  def invalid[A, B](a: A): Validated[A,B] = Validated.Invalid(a)

  /**
   * Similar to invalid, but the error value is wrapped in a non-empty List
   */
  def invalidNel[A, B](a: A): ValidatedNel[A, B] = Validated.Invalid(Nel(a, List.empty))


  /**
   * Evaluates the specified block, catching exceptions of the
   * specified type and returning them on the invalid side of the
   * resulting `Validated`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> import dogs._, Predef._
   * scala> Validated.catchOnly[NumberFormatException] { "foo".toInt }
   * res0: Validated[NumberFormatException, Int] =
   * Invalid(java.lang.NumberFormatException: For input string: "foo")
   * }}}
   */
  def catchOnly[T >: scala.Null <: scala.Throwable]: CatchOnlyPartiallyApplied[T] = new CatchOnlyPartiallyApplied[T]

  final class CatchOnlyPartiallyApplied[T] private[ValidatedFunctions] {
    def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNull[T]): Validated[T, A] =
      try {
        valid(f)
      } catch {
        case t if T.runtimeClass.isInstance(t) =>
          invalid(t.asInstanceOf[T])
      }
  }

  def catchNonFatal[A](f: => A): Validated[scala.Throwable, A] =
    try {
      valid(f)
    } catch {
      case scala.util.control.NonFatal(t) => invalid(t)
    }

  /**
   * Converts a `Try[A]` to a `Validated[Throwable, A]`.
   */
  def fromTry[A](t: Try[A]): Validated[scala.Throwable, A] = t match {
    case Failure(e) => invalid(e)
    case Success(v) => valid(v)
  }

  /**
   * Converts an `Either[A, B]` to an `Validated[A,B]`.
   */
  def fromXor[A, B](e: Xor[A, B]): Validated[A,B] = e.fold(invalid, valid)

  /**
   * Converts an `Option[B]` to an `Validated[A,B]`, where the
   * provided `ifNone` values is returned on the invalid of the
   * `Validated` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): Validated[A,B] = o.cata(valid, invalid[A, B](ifNone))
}
