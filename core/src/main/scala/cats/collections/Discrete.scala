package cats.collections

/**
 * Represent discrete operations that can be performed on A
 */
trait Discrete[@specialized(Specializable.Integral) A] {

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

  /**
   * Returns the inverse of this discrete (succ and pred are flipped).
   */
  def inverse: Discrete[A] =
    Discrete.inverse(this)
}

object Discrete {

  /**
   * Summons an implicit instance of Discrete[A] from the implicit scope.
   */
  @inline final def apply[A](implicit instance: Discrete[A]): Discrete[A] =
    instance

  /**
   * Returns the inverse of the provided discrete (succ and pred are flipped).
   */
  final def inverse[A](discrete: Discrete[A]): Discrete[A] =
    new Discrete[A] {
      override def succ(x: A): A = discrete.pred(x)
      override def pred(x: A): A = discrete.succ(x)
    }

  implicit def integralDiscrete[@specialized(Specializable.Integral) I](implicit I: Integral[I]): Discrete[I] =
    new Discrete[I] {
      import Integral.Implicits._
      override def succ(x: I): I = x + I.one
      override def pred(x: I): I = x - I.one
    }
}
