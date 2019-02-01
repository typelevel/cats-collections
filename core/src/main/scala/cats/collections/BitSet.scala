package cats.collections

import java.lang.Long.bitCount
import scala.annotation.tailrec

import BitSet.{Branch, Empty, Leaf}

/**
 * A fast, immutable BitSet.
 *
 * A Bitset is a specialized type of set that tracks the `Int` values
 * it contains: for each integer value, a BitSet use a single bit to
 * track whether it is present (1) or absent (0). An empty bitset is a
 * (sparse) bitset whose bits are all assumed to be zero.
 *
 * Unlike scala's default immutable this BitSet does not do a full
 * copy on each added value.
 *
 * Interally the implementation is a tree. Its leaves use an
 * Array[Long] value to hold up to 2048 bits, and its branches use an
 * Array[BitSet] to hold up to 32 subtrees. It treats the values it
 * stores as 32-bit unsigned values, which is relevant to the internal
 * addressing methods as well as the order used by `iterator`.
 *
 * The benchmarks suggest this is MUCH faster than Scala's built-in
 * bitset for cases where you may need many modifications and merges,
 * (for example in a BloomFilter).
 */
sealed abstract class BitSet { lhs =>

  /**
   * Offset is the first value that this subtree contains.
   *
   * Offset will always be a multiple of 2048 (2^11).
   *
   * The `offset` is interpreted as a 32-bit unsigned integer. In
   * other words, `(offset & 0xffffffffL)` will return the equivalent
   * value as a signed 64-bit integer (between 0 and 4294967295).
   */
  private[collections] def offset: Int

  /**
   * Limit is the first value beyond the range this subtree
   * supports.
   *
   * In other words, the last value in the subtree's range is `limit - 1`.
   * Like `offset`, `limit` will always be a multiple of 2048.
   *
   * Offset, limit, and height are related:
   *
   *     limit = offset + (32^height) * 2048
   *     limit > offset
   *
   * Like `offset`, `limit` is interpreted as a 32-bit unsigned
   * integer.
   */
  private[collections] def limit: Long

  /**
   * Height represents the number of "levels" this subtree contains.
   *
   * For leaves, height is zero. For branches, height will always be
   * between 1 and 5. This is because a branch with offset=0 and
   * height=5 will have limit=68719476736, which exceeds the largest
   * unsigned 32-bit value we might want to store (4294967295).
   *
   * The calculation `(32^height) * 2048` tells you how many values a
   * subtree contains (i.e. how many bits it holds).
   */
  private[collections] def height: Int

  /**
   * Look for a particular value in the bitset.
   *
   * Returns whether this value's bit is set.
   */
  def apply(n: Int): Boolean

  /**
   * Return a bitset that contains `n` and whose other values are
   * identical to this one's. If this bitset already contains `n` then this
   * method does nothing.
   */
  def +(n: Int): BitSet

  /**
   * Return a bitset that does not contain `n` and whose other values
   * are identical to this one's. If this bitset does not contain `n`
   * then this method does nothing.
   */
  def -(n: Int): BitSet

  /**
   * Return the union of two bitsets as a new immutable bitset.
   *
   * If either bitset contains a given value, the resulting bitset
   * will also contain it.
   */
  def |(rhs: BitSet): BitSet

  /**
   * Return the intersection of two bitsets as a new immutable bitset.
   *
   * The resulting bitset will only contain a value if that value is
   * present in both input bitsets.
   */
  def &(rhs: BitSet): BitSet

  // Internal mutability
  //
  // The following three methods (`+=`, `-=`, and `mutableAdd`) all
  // potentially mutate `this`.
  //
  // These methods are used internally by BitSet's public methods to
  // mutate newly-constructed trees before returning them to the
  // caller. This allows us to avoid unnecessary allocations when we
  // are doing a high-level operation which may result in many
  // separate modifications.

  /**
   * Add a single value `n` to this bitset.
   *
   * This method modifies this bitset. We require that the value `n`
   * is in this node's range (i.e. `offset <= n < limit`).
   */
  private[collections] def +=(n: Int): Unit

  /**
   * Add all values from `rhs` to this bitset.
   *
   * This method modifies this bitset. We require that `this` and
   * `rhs` are aligned (i.e. they both must have the same `offset` and
   * `height`).
   */
  private[collections] def |=(rhs: BitSet): Unit

  /**
   * Add a single value `n` to this bitset to this bitset or to the
   * smallest valid bitset that could contain it.
   *
   * Unlike `+=` this method can be called with `n` outside of this
   * node's range. If the value is in range, the method is equivalent
   * to `+=` (and returns `this`). Otherwise, it wraps `this` in new
   * branches until the node's range is large enough to contain `n`,
   * then adds the value to that node, and returns it.
   */
  private[collections] def mutableAdd(n: Int): BitSet

  /**
   * Return a compacted bitset containing the same values as this one.
   *
   * This method is used to prune out "empty" branches that don't
   * contain values. By default, bitset does not try to remove empty
   * leaves when removing values (since repeatedly checking for this
   * across many deletions would be expensive).
   *
   * The bitset returned will have the same values as the current
   * bitset, but is guaranteed not to contain any empty branches.
   * Empty branches are not usually observable but would result in
   * increased memory usage.
   */
  def compact: BitSet = {
    def recur(x: BitSet): BitSet =
      x match {
        case leaf @ Leaf(_, _) =>
          if (leaf.isEmpty) null else leaf
        case Branch(o, h, cs0) =>
          var i = 0
          var found: BitSet = null
          while (i < 32 && found == null) {
            val c = cs0(i)
            if (c != null) found = recur(c)
            i += 1
          }
          if (found == null) {
            null
          } else {
            val cs1 = new Array[BitSet](32)
            cs1(i - 1) = found
            while (i < 32) {
              val c = cs0(i)
              if (c != null) cs1(i) = recur(c)
              i += 1
            }
            Branch(o, h, cs1)
          }
      }
    val res = recur(this)
    if (res == null) Empty else res
  }

  /**
   * Returns the number of distinct values in this bitset.
   *
   * For branches, this method will return the sum of the sizes of all
   * its subtrees. For leaves it returns the number of bits set in the
   * leaf (i.e. the number of values the leaf contains).
   */
  def size: Int

  /**
   * Iterate across all values in the bitset.
   *
   * Values in the iterator will be seen in "unsigned order" (e.g. if
   * present, -1 will always come last). Here's an abbreviated view of
   * this order in practice:
   *
   *   0, 1, 2, ... 2147483646, 2147483647, -2147483648, -2147483647, ... -1
   *
   * (This "unsigned order" is identical to the tree's internal order.)
   */
  def iterator: Iterator[Int]

  /**
   * Iterate across all values in the bitset in reverse order.
   *
   * The order here is exactly the reverse of `.iterator`.
   */
  def reverseIterator: Iterator[Int]

  /**
   * Present a view of this bitset as a `scala.Set[Int]`.
   *
   * This is provided for compatibility with Scala collections. Many
   * of the set operations are implemented in terms of `BitSet`, but
   * other operations (for example `map`) may copy these values into a
   * different `Set` implementation.
   */
  def toSet: Set[Int] =
    new Set[Int] {
      def contains(i: Int) = lhs(i)
      def iterator = lhs.iterator
      def +(i: Int) = (lhs + i).toSet
      def -(i: Int) = (lhs - i).toSet
      override def empty = Empty.toSet
    }

  /**
   * Returns false this bitset contains values, true otherwise.
   */
  def isEmpty: Boolean =
    this match {
      case Leaf(_, vs) =>
        vs.forall(_ == 0L)
      case Branch(_, _, cs) =>
        cs.forall {
          case null => true
          case c => c.isEmpty
        }
    }

  /**
   * Returns true this bitset contains values, false otherwise.
   */
  def nonEmpty: Boolean = !isEmpty

  /**
   * Produce a string representation of this BitSet.
   *
   * This representation will contain all the values in the bitset.
   * For large bitsets, this operation may be very expensive.
   */
  override def toString: String =
    iterator.map(_.toString).mkString("BitSet(", ", ", ")")

  /**
   * Produce a structured representation of this BitSet.
   *
   * This representation is for internal-use only. It gives a view of
   * how the bitset is encoded in a tree, showing leaves and branches.
   */
  private[collections] def structure: String =
    this match {
      case Branch(o, h, cs) =>
        val s = cs.iterator
          .zipWithIndex
          .filter { case (c, _) => c != null }
          .map { case (c, i) => s"$i -> ${c.structure}" }
          .mkString("Array(", ", ", ")")
        s"Branch($o, $h, $s)"
      case Leaf(o, v) =>
        s"Leaf($o, $v)"
    }

  /**
   * Universal equality.
   *
   * This method will only return true if the right argument is also a
   * `BitSet`. It does not attempt to coerce either argument in any
   * way (unlike Scala collections, for example).
   *
   * Two bitsets can be equal even if they have different underlying
   * tree structure. (For example, one bitset's tree may have empty
   * branches that the other lacks.)
   */
  override def equals(that: Any): Boolean =
    that match {
      case t: BitSet =>
        val it0 = this.iterator
        val it1 = t.iterator
        while (it0.hasNext && it1.hasNext) {
          if (it0.next != it1.next) return false
        }
        it0.hasNext == it1.hasNext
      case _ =>
        false
    }

  /**
   * Universal hash code.
   *
   * Bitsets that are the equal will hash to the same value. As in
   * `equals`, the values present determine the hash code, as opposed
   * to the tree structure.
   */
  override def hashCode: Int = {
    var hash: Int = 1500450271 // prime number
    val it = iterator
    while (it.hasNext) {
      hash = (hash * it.next) + 1023465798 // prime number
    }
    hash
  }
}

object BitSet {

  /**
   * Returns an empty immutable bitset.
   */
  def empty: BitSet = Empty

  /**
   * Singleton value representing an empty bitset.
   */
  final val Empty: BitSet =
    newEmpty(0)

  /**
   * Returns an empty leaf.
   *
   * This is used internally with the assumption that it will be
   * mutated to "add" values to it. In cases where no values need to
   * be added, `empty` should be used instead.
   */
  private[collections] def newEmpty(offset: Int): BitSet =
    Leaf(offset, new Array[Long](32))

  /**
   * Construct an immutable bitset from the given integer values.
   */
  def apply(xs: Int*): BitSet = {
    var bs = newEmpty(0)
    xs.foreach { n =>
      bs = bs.mutableAdd(n)
    }
    bs
  }

  /**
   * Given a value (`n`), and offset (`o`) and a height (`h`), compute
   * the array index used to store the given value's bit.
   */
  @inline private[collections] def index(n: Int, o: Int, h: Int): Int =
    (n - o) >>> (h * 5 + 6)

  /**
   * Given an offset (`o`) and a height (`h`) representing a subtree,
   * compute the array index used this subtree in its parent tree.
   */
  @inline private[collections] def parentOffset(o: Int, h: Int): Int =
    o & -(1 << (h * 5 + 16))

  case class InternalError(msg: String) extends Exception(msg)

  /**
   * Construct the leaf containing the given value `n`.
   *
   * Since leaves have zero height, we can divide a value by 2048
   * (i.e. >> 11) to determine its offset.
   */
  private[collections] def leafFor(n: Int): BitSet = {
    val offset = n >>> 11
    val i = (n - offset) >>> 5
    val j = (n - offset) & 63
    val vs = new Array[Long](32)
    vs(i) = 1L << j
    Leaf(offset, vs)
  }

  /**
   * Return a branch containing the given bitset `b` and value `n`.
   *
   * This method assumes that `n` is outside of the range of `b`. It
   * will return the smallest branch that contains both `b` and `n`.
   */
  private def adoptedPlus(b: BitSet, n: Int): Branch = {
    val h = b.height + 1
    val o = b.offset & -(1 << (h * 5 + 11))
    val cs = new Array[BitSet](32)
    val i = (b.offset - o) >>> (h * 5 + 6)
    cs(i) = b
    val parent = Branch(o, h, cs)
    val j = BitSet.index(n, o, h)
    if (j < 0 || 32 <= j) {
      adoptedPlus(parent, n)
    } else {
      parent += n
      parent
    }
  }

  /**
   * Return a branch containing the given bitsets `b` and `rhs`.
   *
   * This method assumes that `rhs` is at least partially-outside of
   * the range of `b`. It will return the smallest branch that
   * contains both `b` and `rhs`.
   */
  private def adoptedUnion(b: BitSet, rhs: BitSet): Branch = {
    val h = b.height + 1
    val o = b.offset & -(1 << (h * 5 + 11))
    val cs = new Array[BitSet](32)
    val i = (b.offset - o) >>> (h * 5 + 6)
    cs(i) = b
    val parent = Branch(o, h, cs)
    val j = BitSet.index(rhs.offset, o, h)
    if (j < 0 || 32 <= j || rhs.height > parent.height) {
      adoptedUnion(parent, rhs)
    } else {
      parent |= rhs
      parent
    }
  }

  private case class Branch(offset: Int, height: Int, children: Array[BitSet]) extends BitSet {

    @inline private[collections] def limit: Long = offset + (1L << (height * 5 + 11))

    //require(limit > offset, s"$limit > $offset (at height=$height)")

    @inline private[collections] def index(n: Int): Int = (n - offset) >>> (height * 5 + 6)
    @inline private[collections] def valid(i: Int): Boolean = 0 <= i && i < 32
    @inline private[collections] def invalid(i: Int): Boolean = i < 0 || 32 <= i

    def apply(n: Int): Boolean = {
      val i = index(n)
      valid(i) && {
        val c = children(i)
        c != null && c(n)
      }
    }

    def newChild(i: Int): BitSet = {
      val o = offset + i * (1 << height * 5 + 6)
      if (height == 1) BitSet.newEmpty(o)
      else Branch(o, height - 1, new Array[BitSet](32))
    }

    def +(n: Int): BitSet = {
      val i = index(n)
      if (invalid(i)) {
        BitSet.adoptedPlus(this, n)
      } else {
        val c0 = children(i)
        val c1 = if (c0 != null) c0 + n else {
          val cc = newChild(i)
          cc += n
          cc
        }
        // we already had this item
        if (c0 eq c1) this
        else replace(i, c1)
      }
    }

    def replace(i: Int, child: BitSet): Branch = {
      val cs = new Array[BitSet](32)
      System.arraycopy(children, 0, cs, 0, 32)
      cs(i) = child
      copy(children = cs)
    }

    def -(n: Int): BitSet = {
      val i = index(n)
      if (invalid(i)) this
      else {
        val c = children(i)
        if (c == null) this
        else {
          val c1 = c - n
          if (c1 eq c) this // we don't contain n
          else replace(i, c - n)
        }
      }
    }

    def |(rhs: BitSet): BitSet =
      if (height > rhs.height) {
        if (rhs.offset < offset || limit <= rhs.offset) {
          // this branch doesn't contain rhs
          BitSet.adoptedUnion(this, rhs)
        } else {
          // this branch contains rhs, so find its index
          val i = index(rhs.offset)
          val c0 = children(i)
          val c1 =
            if (c0 != null) c0 | rhs
            else if (height == 1) rhs
            else {
              val cc = newChild(i)
              cc |= rhs
              cc
            }
          replace(i, c1)
        }
      } else if (height < rhs.height) {
        // use commuativity to handle this in previous case
        rhs | this
      } else if (offset != rhs.offset) {
        // same height, but non-overlapping
        BitSet.adoptedUnion(this, rhs)
      } else {
        // height == rhs.height, so we know rhs is a Branch.
        val Branch(_, _, rcs) = rhs
        val cs = new Array[BitSet](32)
        var i = 0
        while (i < 32) {
          val x = children(i)
          val y = rcs(i)
          cs(i) = if (x == null) y else if (y == null) x else x | y
          i += 1
        }
        Branch(offset, height, cs)
      }

    def &(rhs: BitSet): BitSet =
      if (height > rhs.height) {
        if (rhs.offset < offset || limit <= rhs.offset) {
          Empty
        } else {
          // this branch contains rhs, so find its index
          val i = index(rhs.offset)
          val c0 = children(i)
          if (c0 != null) c0 & rhs else Empty
        }
      } else if (height < rhs.height) {
        // use commuativity to handle this in previous case
        rhs & this
      } else if (offset != rhs.offset) {
        // same height, but non-overlapping
        Empty
      } else {
        // height == rhs.height, so we know rhs is a Branch.
        val Branch(_, _, rcs) = rhs
        val cs = new Array[BitSet](32)
        var i = 0
        while (i < 32) {
          val x = children(i)
          val y = rcs(i)
          if (x != null && y != null) cs(i) = x & y
          i += 1
        }
        Branch(offset, height, cs)
      }

    private[collections] def +=(n: Int): Unit = {
      val i = index(n)
      //require(valid(i))
      val c0 = children(i)
      if (c0 == null) {
        val c = newChild(i)
        children(i) = c
        c += n
      } else {
        c0 += n
      }
    }

    private[collections] def mutableAdd(n: Int): BitSet = {
      val i = index(n)
      if (valid(i)) {
        val c0 = children(i)
        if (c0 == null) {
          val c = newChild(i)
          children(i) = c
          c += n
        } else {
          c0 += n
        }
        this
      } else {
        BitSet.adoptedPlus(this, n)
      }
    }

    private[collections] def |=(rhs: BitSet): Unit =
      if (height > rhs.height) {
        if (rhs.offset < offset || limit <= rhs.offset) {
          throw InternalError("union outside of branch jurisdiction")
        } else {
          // this branch contains rhs, so find its index
          val i = index(rhs.offset)
          val c0 = children(i)
          if (c0 == null) {
            val c1 = newChild(i)
            c1 |= rhs
            children(i) = c1
          } else {
            c0 |= rhs
          }
        }
      } else if (height < rhs.height) {
        throw InternalError("branch too short for union")
      } else if (offset != rhs.offset) {
        throw InternalError("branch misaligned")
      } else {
        // height == rhs.height, so we know rhs is a Branch.
        val Branch(_, _, rcs) = rhs
        var i = 0
        while (i < 32) {
          val x = children(i)
          val y = rcs(i)
          if (x == null) children(i) = y
          else if (y != null) x |= rcs(i)
          i += 1
        }
      }

    // TODO: optimize
    def iterator: Iterator[Int] =
      children.iterator.flatMap {
        case null => Iterator.empty
        case c => c.iterator
      }

    def reverseIterator: Iterator[Int] =
      children.reverseIterator.flatMap {
        case null => Iterator.empty
        case c => c.reverseIterator
      }

    def size: Int = {
      var i = 0
      var n = 0
      while (i < 32) {
        val c = children(i)
        if (c != null) n += c.size
        i += 1
      }
      n
    }
  }

  private case class Leaf(offset: Int, private val values: Array[Long]) extends BitSet {

    @inline private[collections] def limit: Long = offset + 2048L

    @inline private[collections] def index(n: Int): Int = (n - offset) >>> 6
    @inline private[collections] def bit(n: Int): Int = (n - offset) & 63
    @inline private[collections] def valid(i: Int): Boolean = 0 <= i && i < 2048
    @inline private[collections] def invalid(i: Int): Boolean = i < 0 || 2048 <= i

    def height: Int = 0

    def apply(n: Int): Boolean = {
      val i = index(n)
      (0 <= i && i < 32) && (((values(i) >>> bit(n)) & 1) == 1)
    }

    def arrayCopy: Array[Long] = {
      val vs = new Array[Long](32)
      System.arraycopy(values, 0, vs, 0, 32)
      vs
    }

    def +(n: Int): BitSet = {
      val i = index(n)
      if (0 <= i && i < 32) {
        val mask = 1L << bit(n)
        val vsi = values(i)
        if ((vsi & mask) == 1L) this
        else {
          val vs = arrayCopy
          vs(i) = vsi | mask
          Leaf(offset, vs)
        }
      } else {
        BitSet.adoptedPlus(this, n)
      }
    }

    def -(n: Int): BitSet = {
      val i = index(n)
      if (i < 0 || 32 <= i) {
        this
      } else {
        val mask = (1L << bit(n))
        val vsi = values(i)
        if ((vsi & mask) == 0L) this
        else {
          val vs = arrayCopy
          vs(i) = vsi & (~mask)
          Leaf(offset, vs)
        }
      }
    }

    def size: Int = {
      var c = 0
      var i = 0
      while (i < 32) {
        c += bitCount(values(i))
        i += 1
      }
      c
    }

    def |(rhs: BitSet): BitSet =
      rhs match {
        case Leaf(`offset`, values2) =>
          val vs = new Array[Long](32)
          var i = 0
          while (i < 32) {
            vs(i) = values(i) | values2(i)
            i += 1
          }
          Leaf(offset, vs)
        case _ =>
          BitSet.adoptedUnion(this, rhs)
      }

    def &(rhs: BitSet): BitSet =
      rhs match {
        case Leaf(o, values2) =>
          if (o != offset) {
            Empty
          } else {
            val vs = new Array[Long](32)
            var i = 0
            while (i < 32) {
              vs(i) = values(i) & values2(i)
              i += 1
            }
            Leaf(offset, vs)
          }
        case Branch(_, _, _) =>
          rhs & this
      }

    private[collections] def +=(n: Int): Unit = {
      val i = index(n)
      val j = bit(n)
      values(i) |= (1L << j)
    }

    private[collections] def mutableAdd(n: Int): BitSet = {
      val i = index(n)
      if (0 <= i && i < 32) {
        values(i) |= (1L << bit(n))
        this
      } else {
        BitSet.adoptedPlus(this, n)
      }
    }

    private[collections] def |=(rhs: BitSet): Unit =
      rhs match {
        case Leaf(`offset`, values2) =>
          var i = 0
          while (i < 32) {
            values(i) |= values2(i)
            i += 1
          }
        case _ =>
          throw InternalError("illegal leaf union")
      }

    def iterator: Iterator[Int] =
      new LeafIterator(offset, values)

    def reverseIterator: Iterator[Int] =
      new LeafReverseIterator(offset, values)
  }

  /**
   * Efficient, low-level iterator for BitSet.Leaf values.
   *
   * As mentioned in `BitSet.iterator`, this method will return values
   * in unsigned order (e.g. Int.MaxValue comes before Int.MinValue).
   */
  private class LeafIterator(offset: Int, values: Array[Long]) extends Iterator[Int] {
    var i: Int = 0
    var x: Long = values(0)
    var n: Int = offset

    @tailrec private def search(): Unit =
      if (x == 0 && i < 31) {
        i += 1
        n = offset + i * 64
        x = values(i)
        search()
      } else ()

    private def advance(): Unit = {
      x = x >>> 1
      n += 1
      search()
    }

    search()

    def hasNext: Boolean = x != 0

    def next(): Int = {
      while (x != 0 && (x & 1) == 0) advance()
      if (x == 0) throw new NoSuchElementException("next on empty iterator")
      val res = n
      advance()
      res
    }
  }

  /**
   * Efficient, low-level reversed iterator for BitSet.Leaf values.
   *
   * This class is very similar to LeafIterator but returns values in
   * the reverse order.
   */
  private class LeafReverseIterator(offset: Int, values: Array[Long]) extends Iterator[Int] {
    var i: Int = 31
    var x: Long = values(31)
    var n: Int = offset + (i + 1) * 64 - 1

    @tailrec private def search(): Unit =
      if (x == 0 && i > 0) {
        i -= 1
        n = offset + (i + 1) * 64 - 1
        x = values(i)
        search()
      } else ()

    private def advance(): Unit = {
      x = x << 1
      n -= 1
      search()
    }

    search()

    def hasNext: Boolean = x != 0

    def next(): Int = {
      while (x > 0) advance()
      if (x == 0) throw new NoSuchElementException("next on empty iterator")
      val res = n
      advance()
      res
    }
  }
}
