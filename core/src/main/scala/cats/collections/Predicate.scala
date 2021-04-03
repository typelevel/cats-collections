package cats.collections

import algebra.lattice.Bool
import cats._
import cats.data.Kleisli

/**
 * An intensional set, which is a set which instead of enumerating its
 * elements as a extensional set does, it is defined by a predicate
 * which is a test for membership.
 *
 * All combinators in this class are implemented in a stack-safe way.
 */
sealed abstract class Predicate[-A] extends scala.Function1[A, Boolean] { self =>
  def run: Kleisli[Eval, A, Boolean]

  /**
   * Returns true if the value satisfies the predicate.
   */
  def apply(a: A): Boolean = run(a).value

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def union[B <: A](other: Predicate[B]): Predicate[B] =
    self match {
      case Predicate.Empty => other
      case Predicate.Everything => self
      case _ =>
      other match {
        case Predicate.Empty => self
        case Predicate.Everything => other
        case _ => Predicate.Lift {
          self.run.flatMap(if (_) Predicate.True else other.run)
        }
      }
    }

  /**
   * returns a predicate which is the union of this predicate and another
   */
  def |[B <: A](other: Predicate[B]): Predicate[B] = self union other

  /**
   * returns a predicate which is the intersection of this predicate and another
   */
  def intersection[B <: A](other: Predicate[B]): Predicate[B] =
    self match {
      case Predicate.Empty => self
      case Predicate.Everything => other
      case _ =>
        other match {
        case Predicate.Empty => other
        case Predicate.Everything => self
        case _ => Predicate.Lift {
          self.run.flatMap(if (_) other.run else Predicate.False)
        }
      }
    }

  /**
   * returns a predicate which is the intersection of this predicate and another
   */
  def &[B <: A](other: Predicate[B]): Predicate[B] = self intersection other

  /**
   * Returns true if the value satisfies the predicate.
   */
  def contains(a: A): Boolean = apply(a)

  /**
   * Returns the predicate which is the the difference of another predicate removed from this predicate
   */
  def diff[B <: A](remove: Predicate[B]): Predicate[B] = self intersection remove.negate

  /**
   * Returns the predicate which is the the difference of another predicate removed from this predicate
   */
  def -[B <: A](remove: Predicate[B]): Predicate[B] = self diff remove

  /**
   * Return the opposite predicate
   */
  def negate: Predicate[A]

  /**
   * Return the opposite predicate
   */
  def unary_!(): Predicate[A] = negate

  /**
   * Compose the predicate with a function.
   *
   * A value is a member of the resulting predicate iff its image through f is a
   * member of this predicate.
   */
  def contramap[B](f: B => A): Predicate[B]

  /**
   * Alias for contramap.
   */
  final override def compose[B](f: B => A): Predicate[B] = contramap(f)
}

object Predicate extends PredicateInstances {
  private val True = Kleisli.liftF(Eval.True)
  private val False = Kleisli.liftF(Eval.False)

  private[collections] case object Empty extends Predicate[Any] {
    override def run: Kleisli[Eval, Any, Boolean] = Predicate.False
    override def negate: Predicate[Any] = Everything
    override def contramap[B](f: B => Any): Predicate[B] = this
  }

  private[collections] case object Everything extends Predicate[Any] {
    override def run: Kleisli[Eval, Any, Boolean] = Predicate.True
    override def negate: Predicate[Any] = Empty
    override def contramap[B](f: B => Any): Predicate[B] = this
  }

  private[collections] final case class Lift[A](run: Kleisli[Eval, A, Boolean]) extends Predicate[A] {
    override def negate: Predicate[A] = Negate(this)
    override def contramap[B](f: B => A): Predicate[B] = Lift(run.compose(b => Eval.now(f(b))))
  }

  private[collections] final case class Negate[A](negate: Predicate[A]) extends Predicate[A] {
    override def run: Kleisli[Eval, A, Boolean] = negate.run.map(!_)
    override def contramap[B](f: B => A): Predicate[B] = Negate(negate contramap f)
  }

  /**
   * build a set from a membership function.
  */
  def apply[A](p: A => Boolean): Predicate[A] = Lift {
    Kleisli(a => if (p(a)) Eval.True else Eval.False)
  }

  /**
   * The empty set: a predicate that rejects all values.
   */
  def empty[A]: Predicate[A] = Empty

  /**
   * The set of everything: a predicate that accepts all values.
   */
  def everything[A]: Predicate[A] = Everything
}

trait PredicateInstances {
  implicit def catsCollectionsPredicateContravariantMonoidal: ContravariantMonoidal[Predicate] = new ContravariantMonoidal[Predicate] {
    override def contramap[A, B](fb: Predicate[A])(f: B => A): Predicate[B] =
      fb.contramap(f)
    override def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
      fa.contramap[(A,B)](_._1) union fb.contramap(_._2)
    override def unit: Predicate[Unit] = Predicate.empty
  }

  implicit def catsCollectionsPredicateMonoid[A]: Monoid[Predicate[A]] = new Monoid[Predicate[A]] {
    override def empty: Predicate[A] = Predicate.empty
    override def combine(l: Predicate[A], r: Predicate[A]): Predicate[A] = l union r
  }

  implicit def catsCollectionsPredicateBool[A]: Bool[Predicate[A]] = new Bool[Predicate[A]] {
    override def one: Predicate[A] = Predicate.everything
    override def zero: Predicate[A] = Predicate.empty
    override def complement(x: Predicate[A]): Predicate[A] = x.negate
    override def and(l: Predicate[A], r: Predicate[A]): Predicate[A] = l intersection r
    override def or(l: Predicate[A], r: Predicate[A]): Predicate[A] = l union r

  }

  implicit val catsCollectionsPredicateMonoidK: MonoidK[Predicate] = new MonoidK[Predicate] {
    override def empty[A]: Predicate[A] = Predicate.empty
    override def combineK[A](l: Predicate[A], r: Predicate[A]): Predicate[A] = l union r
  }
}
