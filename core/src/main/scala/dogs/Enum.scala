package dogs

import Predef._
import cats.Order

/**
 * Represent discrete operations that can be performed on A
 */
trait Enum[A] {

  /**
   * Return the successor of x.
   */
  def succ(x: A): A

  /**
   * Returns the predecessor of x.
   */
  def pred(x: A): A

  /**
   * Returns true if x and y are consecutive.
   */
  def adj(x: A, y: A): Boolean = succ(x) == y
}

object Enum {
  implicit val intEnum: Enum[Int] = new Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
  }

  implicit val bigIntEnum: Enum[BigInt] = new Enum[BigInt] {
    override def succ(x: BigInt): BigInt = x + 1
    override def pred(x: BigInt): BigInt = x - 1
  }
}

