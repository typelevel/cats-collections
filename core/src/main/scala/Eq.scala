package dogs

import Predef._
import simulacrum._

@typeclass
trait Eq[A] { self =>

  @op("===")
  def eqv(l: A, r: A): Boolean

  @op("=!=")
  def neq(l: A, r: A): Boolean = !eqv(l,r)

  def contramap[B](f: B => A): Eq[B] = Eq((l,r) => self.eqv(f(l), f(r)))
}

object Eq {
  def apply[A](f: (A,A) => Boolean) = new Eq[A] {
    override def eqv(l: A, r: A) = f(l,r)
  }

  def fromEquals[A]: Eq[A] = new Eq[A] {
    override def eqv(l: A, r: A) = l == r
  }

  def always[A]: Eq[A] = new Eq[A] {
    override def eqv(l: A, r: A) = true
  }
}

