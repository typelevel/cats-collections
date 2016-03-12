package dogs

object Predef {
  import scala.annotation.{implicitNotFound}
  import scala.{deprecated, inline}

  @inline def identity[A](a: A): A = a
  def implicitly[A](implicit a: A): A = a

  type Any = scala.Any
  type AnyRef = scala.AnyRef
  type AnyVal = scala.AnyVal
  type BigInt = scala.BigInt
  type BigDecimal = scala.BigDecimal
  type Boolean = scala.Boolean
  type Byte = scala.Byte
  type Char = scala.Char
  type Double = scala.Double
  type Float = scala.Float
  type Int = scala.Int
  type Long = scala.Long
  type Nothing = scala.Nothing
  type PartialFunction[A,B] = scala.PartialFunction[A,B]
  type Product = scala.Product
  type Serializable = scala.Serializable
  type Short = scala.Short
  type String = java.lang.String
  type Unit = scala.Unit
  type StringContext = scala.StringContext

  final val BigInt = scala.BigInt
  final val BigDecimal = scala.BigDecimal
  final val Boolean = scala.Boolean
  final val Byte = scala.Byte
  final val Char = scala.Char
  final val Double = scala.Double
  final val Float = scala.Float
  final val Int = scala.Int
  final val Long = scala.Long
  final val Short = scala.Short
  final val Unit = scala.Unit
  final val StringContext = scala.StringContext

  type tailrec = scala.annotation.tailrec

  /**
   * An instance of `A <:< B` witnesses that `A` is a subtype of `B`.
   * Requiring an implicit argument of the type `A <:< B` encodes
   * the generalized constraint `A <: B`.
   *
   * @note we need a new type constructor `<:<` and evidence `conforms`,
   * as reusing `Function1` and `identity` leads to ambiguities in
   * case of type errors (`any2stringadd` is inferred)
   *
   * To constrain any abstract type T that's in scope in a method's
   * argument list (not just the method's own type parameters) simply
   * add an implicit argument of type `T <:< U`, where `U` is the required
   * upper bound; or for lower-bounds, use: `L <:< T`, where `L` is the
   * required lower bound.
   *
   * In part contributed by Jason Zaugg.
   */
  @implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
  sealed abstract class <:<[-From, +To] extends (From => To) with Serializable
  private[this] final val singleton_<:< = new <:<[Any,Any] { def apply(x: Any): Any = x }
  // The dollar prefix is to dodge accidental shadowing of this method
  // by a user-defined method of the same name (SI-7788).
  // The collections rely on this method.
  implicit def $conforms[A]: A <:< A = singleton_<:<.asInstanceOf[A <:< A]

  @deprecated("Use `implicitly[T <:< U]` or `identity` instead.", "2.11.0")
  def conforms[A]: A <:< A = $conforms[A]

  /** An instance of `A =:= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<:<` for expressing subtyping constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable
  private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
  object =:= {
     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }

  implicit final class ArrowAssoc[A](val self: A) extends AnyVal {
    import scala.Tuple2
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }

  // stew: should we make these use Show?
  def print(x: Any) = scala.Console.print(x)
  def println() = scala.Console.println()
  def println(x: Any) = scala.Console.println(x)
}
