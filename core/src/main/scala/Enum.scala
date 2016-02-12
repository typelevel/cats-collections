package dogs

import Predef._
import Order._

/**
 * Represent discrete operations that can be performed on A
 */
trait Enum[A] extends Order[A] { // STU: not sure if extending order is wise

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
    override def apply(l: Int, r: Int): Ordering = if (l < r) LT else if (l > r) GT else EQ
  }

  implicit val bigIntEnum: Enum[BigInt] = new Enum[BigInt] {
    override def succ(x: BigInt): BigInt = x + 1
    override def pred(x: BigInt): BigInt = x - 1
    override def apply(l: BigInt, r: BigInt): Ordering = if (l < r) LT else if (l > r) GT else EQ
  }
}

