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
  implicit def integralDiscrete[@specialized(Specializable.Integral) I](
      implicit I: Integral[I]): Discrete[I] = 
  new Discrete[I] {
    import Integral.Implicits._
    override def succ(x: I): I = x + I.one
    override def pred(x: I): I = x - I.one
  }
}

