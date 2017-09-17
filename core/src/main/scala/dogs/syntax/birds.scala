package dogs.syntax

import scala.{AnyVal,Unit}

trait BedazzleBirds {
  implicit def toBirdOps[A](a: A): BirdOps[A] = new BirdOps(a)
}

class BirdOps[A](val a: A) extends AnyVal {

  /**
   * also called Thrush, see
   * http://www.angelfire.com/tx4/cus/combinator/birds.html
   */
  def |>[B](f: A => B): B = f(a)

  def unsafeTap(f: A => Unit): A = {
    f(a)
    a
  }

  /**
   * also called Kestrel, see
   * http://www.angelfire.com/tx4/cus/combinator/birds.html
   */
  def <|(f: A => Unit): A = {
    f(a)
    a
  }

  def $[B](f: A => B): B = f(a)
}
