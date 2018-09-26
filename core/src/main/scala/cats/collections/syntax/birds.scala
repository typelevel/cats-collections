package cats.collections.syntax

import scala.{AnyVal,Unit}

@deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
trait BedazzleBirds {
  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  implicit def toBirdOps[A](a: A): BirdOps[A] = new BirdOps(a)
}

@deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
class BirdOps[A](val a: A) extends AnyVal {

  /**
   * also called Thrush, see
   * http://www.angelfire.com/tx4/cus/combinator/birds.html
   */
  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  def |>[B](f: A => B): B = f(a)

  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  def unsafeTap(f: A => Unit): A = {
    f(a)
    a
  }

  /**
   * also called Kestrel, see
   * http://www.angelfire.com/tx4/cus/combinator/birds.html
   */
  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  def <|(f: A => Unit): A = {
    f(a)
    a
  }

  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  def $[B](f: A => B): B = f(a)
}
