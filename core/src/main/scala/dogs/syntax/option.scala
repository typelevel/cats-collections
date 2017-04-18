package dogs
package syntax

trait OptionSyntax {
  def none[A] = None()
  implicit def optionIdSyntax[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
  implicit def optionSyntax[A](oa: Option[A]): OptionOps[A] = new OptionOps(oa)
}

final class OptionIdOps[A](val a: A) extends AnyVal {
  def some: Option[A] = Option(a)
}

final class OptionOps[A](val oa: Option[A]) extends AnyVal {
  def toLeft[B](b: => B): scala.Either[A, B] = oa.cata(scala.Left(_), scala.Right(b))
  def toRight[B](b: => B): scala.Either[B, A] = oa.cata(scala.Right(_), scala.Left(b))
  def toInvalid[B](b: => B): Validated[A, B] = oa.cata(Validated.invalid, Validated.valid(b))
  def toInvalidNel[B](b: => B): ValidatedNel[A, B] = oa.cata(Validated.invalidNel, Validated.Valid(b))
  def toValid[B](b: => B): Validated[B, A] = oa.cata(Validated.valid, Validated.Invalid(b))
  def toValidNel[B](b: => B): ValidatedNel[B, A] = oa.cata(Validated.valid, Validated.invalidNel(b))
}

