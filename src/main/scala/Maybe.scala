package dogs

import scala.util.control.NonFatal
import scala.reflect.ClassTag
import Ordering._
import cats._
import cats.data._

/** An optional value
 *
 * A `Maybe[A]` will either be a wrapped `A` instance (`There[A]`), or a lack of underlying
 * `A` instance (`NotThere[A]`).
 *
 * `Maybe[A]` is isomorphic to `Option[A]`, however there are some differences between
 * the two. `Maybe` is invariant in `A` while `Option` is covariant. `Maybe[A]` does not expose
 * an unsafe `get` operation to access the underlying `A` value (that may not exist) like
 * `Option[A]` does. `Maybe[A]` does not come with an implicit conversion to `Iterable[A]` (a
 * trait with over a dozen super types).
 */
sealed abstract class Maybe[A] {
  import Maybe._

  /** Catamorphism.
   * Run the given function on the underlying value if present, otherwise return
   * the provided fallback value */
  final def cata[B](f: A => B, b: => B): B =
    this match {
      case There(a) => f(a)
      case NotThere() => b
    }

  /** Return the underlying value if present, otherwise the provided fallback value */
  final def getOrElse(a: => A): A =
    cata(identity, a)

  /** alias for [[getOrElse]] */
  final def |(a: => A): A =
    getOrElse(a)

  /** Turn the underlying value into a failure validation if present, otherwise
   * return a success validation with the provided fallback value */
  final def toFailure[B](b: => B): Validated[A, B] =
    cata(Validated.Invalid(_), Validated.Valid(b))

  /** Turn the underlying value into a success validation if present, otherwise
   * return a failure validation with the provided fallback value */
  final def toSuccess[B](b: => B): Validated[B, A] =
    cata(Validated.Valid(_), Validated.Invalid(b))

  /** Turn the underlying value into a left disjunction if present, otherwise
   * return a right disjunction with the provided fallback value */
  final def toLeft[B](b: => B): A Xor B =
    cata(Xor.left(_), Xor.right(b))

  /** alias for [[toLeft]] */
  final def <\/[B](b: => B): A Xor B =
    toLeft(b)

  /** Turn the underlying value into a right disjunction if present, otherwise
   * return a left disjunction with the provided fallback value */
  final def toRight[B](b: => B): B Xor A =
    cata(Xor.right(_), Xor.left(b))

  /** alias for [[toRight]] */
  final def \/>[B](b: => B): B Xor A =
    toRight(b)

  /** True if an underlying value is present */
  final def isThere: Boolean =
    cata(_ => true, false)

  /** True if no underlying value is present */
  final def isNotThere: Boolean =
    cata(_ => false, true)

  final def map[B](f: A => B): Maybe[B] =
    cata(f andThen there[B], notThere[B])

  final def flatMap[B](f: A => Maybe[B]) =
    cata(f, notThere[B])

  /** Convert to a standard library `Option` */
  final def toOption: Option[A] =
    cata(Some(_), None)

  /** Return this instance if it is a [[Maybe.There]], otherwise the provided fallback */
  final def orElse(oa: => Maybe[A]): Maybe[A] =
    cata(_ => this, oa)

  final def coflatten: Maybe[Maybe[A]] = map(there)

  final def coflatMap[B](f: Maybe[A] => B): Maybe[B] =
    map(_ => f(this))

  final def zip[B](fb: Maybe[B]): Maybe[(A, B)] =
    for {
      a <- this
      b <- fb
    } yield (a, b)

  final def zipWith[B, C](fb: Maybe[B])(f: (A, B) => C): Maybe[C] =
    for {
      a <- this
      b <- fb
    } yield f(a, b)

  final def filter(f: A => Boolean): Maybe[A] =
    flatMap(a => if (f(a)) this else notThere)

  final def filterNot(f: A => Boolean): Maybe[A] =
    filter(f.andThen(!_))

  /** Return `true` if this is a [[Maybe.NotThere]] or if this is a [[Maybe.There]]
   * and the underlying value satisfies the provided predicate */
  final def forall(f: A => Boolean): Boolean =
    cata(f, true)

  /** Return `true` if this is a [[Maybe.There]] and the underlying value
   * satisfies the provided predicate */
  final def exists(f: A => Boolean): Boolean =
    cata(f, false)

  /** Return the underlying value if present, otherwise the monoid zero */
  final def orZero(implicit F: Monoid[A]): A =
    getOrElse(F.empty)

  /** alias for [[orZero]] */
  final def unary_~(implicit z: Monoid[A]): A =
    orZero

  /**
   * Return the underlying value wrapped in type `F` if present, otherwise the
   * notThere value for type `F` */
  final def orNotThere[F[_]](implicit F: Applicative[F], G: MonoidK[F]): F[A] =
    cata(F.pure(_), G.empty)
}

object Maybe extends MaybeInstances with MaybeFunctions {

  final case class NotThere[A]() extends Maybe[A]

  final case class There[A](a: A) extends Maybe[A]
}

sealed trait MaybeFunctions {
  import Maybe._

  /** Wrap a value in There, or return NotThere if the value is null */
  final def fromNullable[A](a: A): Maybe[A] =
    if (null == a) notThere else there(a)

  final def notThere[A]: Maybe[A] = NotThere()

  final def there[A](a: A): Maybe[A] = There(a)

  final def fromOption[A](oa: Option[A]): Maybe[A] =
    oa.fold(notThere[A])(there)

  def fromTryCatchNonFatal[T](a: => T): Maybe[T] = try {
    there(a)
  } catch {
    case NonFatal(t) => notThere
  }
}

sealed abstract class MaybeInstances {
  import Maybe._

  implicit def maybeEqual[A : Eq]: Eq[Maybe[A]] = new MaybeEqual[A] {
    def A = implicitly
  }

  implicit def maybeOrder[A : Order]: Order[Maybe[A]] = new Order[Maybe[A]] with MaybeEqual[A] {
    def A = implicitly

    override def compare(fa1: Maybe[A], fa2: Maybe[A]) =
      fa1.cata(a1 => fa2.cata(a2 => Order[A].compare(a1, a2), 1), fa2.cata(_ => -1, 0))
  }

  implicit def maybeShow[A](implicit A: Show[A]): Show[Maybe[A]] =
    Show.show(_.cata(a => s"There(${A.show(a)}", "NotThere"))

  implicit def maybeMonoid[A](implicit A: Semigroup[A]): Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    def combine(fa1: Maybe[A], fa2: Maybe[A]) =
      fa1.cata(
        a1 => fa2.cata(a2 => there(A.combine(a1, a2)), fa1),
        fa2.cata(_ => fa2, notThere))

    def empty = notThere
  }

  implicit val maybeInstance: Traverse[Maybe] with MonadCombine[Maybe] with CoflatMap[Maybe] = new Traverse[Maybe] with MonadCombine[Maybe] with CoflatMap[Maybe] {
    override def pure[A](a: A) = there(a)

    override def ap[A, B](fa: Maybe[A])(mf: Maybe[A => B]) =
      mf.cata(f => fa.cata(f andThen there, notThere), notThere)

    override def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa flatMap f

    override def map[A, B](fa: Maybe[A])(f: A => B) = fa map f

    override def traverse[F[_], A, B](fa: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa.cata(a => F.map(f(a))(there), F.pure(notThere))

    override def empty[A]: Maybe[A] = Maybe.notThere

    override def combine[A](a: Maybe[A], b: Maybe[A]) = a orElse b

    override def foldLeft[A,B](fa: Maybe[A], b: B)(f: (B, A) => B): B =
      fa.cata(f(b, _), b)

    override def foldRight[A, B](fa: Maybe[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
      fa.cata(f(_, b), b)

    override def coflatMap[A, B](fa: Maybe[A])(f: Maybe[A] => B) =
      fa.coflatMap(f)

    override def coflatten[A](a: Maybe[A]) =
      a.coflatten

    override def filter[A](fa: Maybe[A])(f: A => Boolean): Maybe[A] =
      fa.filter(f)
  }
}

private sealed trait MaybeEqual[A] extends Eq[Maybe[A]] {
  implicit def A: Eq[A]

  override final def eqv(fa1: Maybe[A], fa2: Maybe[A]) =
    fa1.cata(
      a1 => fa2.cata(a2 => A.eqv(a1, a2), false),
      fa2.cata(_ => false, true))
}
