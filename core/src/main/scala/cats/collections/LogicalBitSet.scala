package cats.collections

sealed abstract class LogicalBitSet { lhs =>

  import LogicalBitSet.{Absent, Present, FullSize}

  def apply(n: Int): Boolean =
    this match {
      case Present(bits) => bits(n)
      case Absent(bits) => !bits(n)
    }

  override def toString: String =
    this match {
      case Present(xs) => s"Present($xs, ${xs.structure})"
      case Absent(xs) => s"Absent($xs, ${xs.structure})"
    }

  override def equals(that: Any): Boolean =
    (this, that) match {
      case (Present(xs), Present(ys)) =>
        xs == ys
      case (Absent(xs), Absent(ys)) =>
        xs == ys
      case (p @ Present(xs), a @ Absent(ys)) =>
        p.size + a.size == FullSize && !(xs intersects ys)
      case (a @ Absent(xs), p @ Present(ys)) =>
        p.size + a.size == FullSize && !(xs intersects ys)
      case _ =>
        false
    }

  override def hashCode: Int =
    this match {
      case Present(bits) => bits.hashCode ^ 0xbb32f9a2
      case Absent(bits) => bits.hashCode ^ 0x5128937f
    }

  def size: Long =
    this match {
      case Present(bits) => bits.size
      case Absent(bits) => FullSize - bits.size
    }

  def compact: LogicalBitSet =
    this match {
      case Present(bits) => Present(bits.compact)
      case Absent(bits) => Absent(bits.compact)
    }

  def +(n: Int): LogicalBitSet =
    this match {
      case Present(bits) => Present(bits + n)
      case Absent(bits) => Absent(bits - n)
    }

  def -(n: Int): LogicalBitSet =
    this match {
      case Present(bits) => Present(bits - n)
      case Absent(bits) => Absent(bits + n)
    }

  def unary_~ : LogicalBitSet =
    this match {
      case Present(bits) => Absent(bits)
      case Absent(bits) => Present(bits)
    }

  def &(rhs: LogicalBitSet): LogicalBitSet =
    (lhs, rhs) match {
      case (Present(bits0), Present(bits1)) => Present(bits0 & bits1)
      case (Absent(bits0), Absent(bits1)) => Absent(bits0 | bits1)
      case (Present(bits0), Absent(bits1)) => Present(bits0 -- bits1)
      case (Absent(bits0), Present(bits1)) => Present(bits1 -- bits0)
    }

  def |(rhs: LogicalBitSet): LogicalBitSet =
    (lhs, rhs) match {
      case (Present(bits0), Present(bits1)) => Present(bits0 | bits1)
      case (Absent(bits0), Absent(bits1)) => Absent(bits0 & bits1)
      case (Present(bits0), Absent(bits1)) => Absent(bits1 -- bits0)
      case (Absent(bits0), Present(bits1)) => Absent(bits0 -- bits1)
    }

  def ^(rhs: LogicalBitSet): LogicalBitSet =
    (lhs, rhs) match {
      case (Present(bits0), Present(bits1)) => Present(bits0 ^ bits1)
      case (Absent(bits0), Absent(bits1)) => Present(bits0 ^ bits1)
      case (Present(bits0), Absent(bits1)) => Absent(bits0 ^ bits1)
      case (Absent(bits0), Present(bits1)) => Absent(bits0 ^ bits1)
    }

  def --(rhs: LogicalBitSet): LogicalBitSet =
    (lhs, rhs) match {
      case (Present(bits0), Present(bits1)) => Present(bits0 -- bits1)
      case (Present(bits0), Absent(bits1)) => Present(bits0 & bits1)
      case (Absent(bits0), Present(bits1)) => Absent(bits0 | bits1)
      case (Absent(bits0), Absent(bits1)) => Present(bits1 -- bits0)
    }
}

object LogicalBitSet {

  final val FullSize = 4294967296L

  def empty: LogicalBitSet = Empty
  val Empty: LogicalBitSet = Present(BitSet.empty)

  def full: LogicalBitSet = Full
  val Full: LogicalBitSet = Absent(BitSet.empty)

  def apply(ns: Int*): LogicalBitSet =
    Present(BitSet(ns: _*))
  def apply(bitSet: BitSet): LogicalBitSet =
    Present(bitSet)

  def allExcept(ns: Int*): LogicalBitSet =
    Absent(BitSet(ns: _*))
  def allExcept(bitSet: BitSet): LogicalBitSet =
    Absent(bitSet)

  case class Present(bits: BitSet) extends LogicalBitSet
  case class Absent(bits: BitSet) extends LogicalBitSet
}
