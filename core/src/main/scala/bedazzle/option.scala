package dogs.bedazzle

import scala.{AnyVal, None, Option, Some}

trait OptionBedazzle {
  def none[A]: Option[A] = None

  implicit def someBedazzle[A](a: A) = new SomeBedazzle(a)
}

class SomeBedazzle[A](val a: A) extends AnyVal {
  def some: Option[A] = Some(a)
}
