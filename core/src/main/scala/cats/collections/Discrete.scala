package cats.collections


/**
 * Represent discrete operations that can be performed on A
 */
trait Discrete[A] {

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

object Discrete {
  implicit val intDiscrete: Discrete[Int] = new Discrete[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
  }

  implicit val bigIntDiscrete: Discrete[BigInt] = new Discrete[BigInt] {
    override def succ(x: BigInt): BigInt = x + 1
    override def pred(x: BigInt): BigInt = x - 1
  }
}

