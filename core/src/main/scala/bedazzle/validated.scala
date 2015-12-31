package dogs
package bedazzle

import Predef._

trait ValidatedSyntax {
  implicit def validatedIdSyntax[A](a: A): ValidatedIdSyntax[A] = new ValidatedIdSyntax(a)
}

final class ValidatedIdSyntax[A](val a: A) extends AnyVal {
  def valid[B]: Validated[B, A] = Validated.valid(a)
  def validNel[B]: ValidatedNel[B, A] = Validated.valid(a)
  def invalid[B]: Validated[A, B] = Validated.invalid(a)
  def invalidNel[B]: ValidatedNel[A, B] = Validated.invalidNel(a)
}
