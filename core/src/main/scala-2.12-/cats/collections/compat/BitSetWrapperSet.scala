package cats.collections
package compat

class BitSetWrapperSet(bitset: BitSet) extends Set[Int] {
  def contains(i: Int) = bitset(i)
  def iterator = bitset.iterator
  def +(i: Int) = new BitSetWrapperSet(bitset + i)
  def -(i: Int) = new BitSetWrapperSet(bitset - i)
  override def empty = BitSet.Empty.toSet
}
