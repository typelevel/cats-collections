package dogs
package bedazzle

import Predef._

trait XorSyntax {
  implicit def xorIdSyntax[A](a: A): XorIdOps[A] = new XorIdOps(a)
}

final class XorIdOps[A](val a: A) extends AnyVal {
  def left[B]: A Xor B = Xor.Left(a)
  def right[B]: B Xor A = Xor.Right(a)
}
