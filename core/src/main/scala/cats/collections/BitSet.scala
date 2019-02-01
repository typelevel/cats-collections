package cats.collections

import java.lang.Long.bitCount
import scala.annotation.tailrec

import BitSet.{Branch, Empty, Leaf}

/**
 * A fast immutable BitSet which unlike scala's default immutable BitSet does
 * not do a full copy on each add. Interally it is based on a Tree of Array[Long].
 * The benchmarks suggest this is MUCH faster for cases where you need many modifications
 * and merges, e.g. a BloomFilter.
 */
sealed abstract class BitSet { lhs =>

  private[collections] def offset: Int
  private[collections] def limit: Long
  private[collections] def height: Int

  def apply(n: Int): Boolean

  def +(n: Int): BitSet
  def -(n: Int): BitSet

  def |(rhs: BitSet): BitSet
  def &(rhs: BitSet): BitSet

  private[collections] def +=(n: Int): Unit
  private[collections] def |=(rhs: BitSet): Unit
  private[collections] def mutableAdd(n: Int): BitSet

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

  def size: Int
  def iterator: Iterator[Int]
  def reverseIterator: Iterator[Int]

  /**
   * Implement a wrapper around this set
   */
  def toSet: Set[Int] =
    new Set[Int] {
      def contains(i: Int) = lhs(i)
      def iterator = lhs.iterator
      def +(i: Int) = (lhs + i).toSet
      def -(i: Int) = (lhs - i).toSet
      override def empty = Empty.toSet
    }

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

  def nonEmpty: Boolean = !isEmpty

  override def toString: String =
    iterator.map(_.toString).mkString("BitSet(", ", ", ")")

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
  def empty: BitSet = Empty

  final val Empty: BitSet =
    newEmpty(0)

  private[collections] def newEmpty(offset: Int): BitSet =
    Leaf(offset, new Array[Long](32))

  def apply(xs: Int*): BitSet = {
    var bs = newEmpty(0)
    xs.foreach { n =>
      bs = bs.mutableAdd(n)
    }
    bs
  }

  @inline private[collections] def index(n: Int, o: Int, h: Int): Int =
    (n - o) >>> (h * 5 + 6)

  @inline private[collections] def parentOffset(o: Int, h: Int): Int =
    o & -(1 << (h * 5 + 16))

  case class InternalError(msg: String) extends Exception(msg)

  private[collections] def leafFor(n: Int): BitSet = {
    val offset = n >>> 11
    val i = (n - offset) >>> 5
    val j = (n - offset) & 63
    val vs = new Array[Long](32)
    vs(i) = 1L << j
    Leaf(offset, vs)
  }

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

    @inline private[collections] def limit: Long = offset + (1L << height * 5 + 11)

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
