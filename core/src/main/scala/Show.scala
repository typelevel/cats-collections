package dogs

import dogs.Predef._
import simulacrum.typeclass

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
@typeclass trait Show[A] extends Serializable { self =>
  def show(a: A): String

  def contramap[B](f: B => A): Show[B] = new Show[B] {
    override def show(b: B): String = self.show(f(b))
  }
}

object Show {
  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }
}
