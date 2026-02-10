/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.collections

import cats.{
  Alternative,
  Applicative,
  CoflatMap,
  Eq,
  Eval,
  Functor,
  FunctorFilter,
  Monad,
  Monoid,
  Order,
  PartialOrder,
  Show,
  Traverse
}

import scala.annotation.tailrec

/**
 * A sequence data structure combining O(1) concatenation with efficient iteration and indexed access.
 *
 * Internally represented as a tree of array chunks. Like [[cats.data.Chain]], ChunkedSeq provides O(1) prepend, append,
 * and concat operations. Unlike Chain, ChunkedSeq uses bounded-size array chunks for cache-friendly O(N) iteration and
 * provides indexed access.
 *
 * Key complexity:
 *   - prepend, append, concat: O(1)
 *   - uncons, unsnoc: O(log N) amortized, stack-safe
 *   - headOption, lastOption: O(log N), stack-safe
 *   - get, getUnsafe: O(log N) for balanced trees
 *   - foldLeft, strictFoldRight, toIterator: O(N), stack-safe
 *   - map, flatMap, filter: O(N), stack-safe
 *   - size, isEmpty: O(1)
 */
sealed abstract class ChunkedSeq[+A] {

  /**
   * The number of elements. O(1)
   */
  def size: Long

  /**
   * Returns true if there are no elements. O(1)
   */
  def isEmpty: Boolean

  /**
   * Returns true if there is at least one element. O(1)
   */
  def nonEmpty: Boolean

  /**
   * Put an item on the front. O(1)
   */
  final def prepend[A1 >: A](a1: A1): ChunkedSeq[A1] =
    ChunkedSeq.concatSafe(ChunkedSeq.one(a1), this)

  /**
   * Put an item on the back. O(1)
   */
  final def append[A1 >: A](a1: A1): ChunkedSeq[A1] =
    ChunkedSeq.concatSafe(this, ChunkedSeq.one(a1))

  /**
   * Alias for prepend. O(1)
   */
  final def ::[A1 >: A](a1: A1): ChunkedSeq[A1] = prepend(a1)

  /**
   * Alias for append. O(1)
   */
  final def :+[A1 >: A](a1: A1): ChunkedSeq[A1] = append(a1)

  /**
   * Concatenate two ChunkedSeqs. O(1)
   */
  final def ++[A1 >: A](that: ChunkedSeq[A1]): ChunkedSeq[A1] =
    ChunkedSeq.concatSafe(this, that)

  /**
   * This is like headOption and tailOption in one call. O(log N) amortized, stack-safe.
   */
  final def uncons: Option[(A, ChunkedSeq[A])] = {
    if (isEmpty) None
    else {
      var current: ChunkedSeq[A] = this
      var rights: ChunkedSeq[A] = ChunkedSeq.empty
      var result: (A, ChunkedSeq[A]) = null
      while (result eq null) {
        if (current.isInstanceOf[ChunkedSeq.Chunk[_]]) {
          val c = current.asInstanceOf[ChunkedSeq.Chunk[A]]
          val head = c.arr(c.offset).asInstanceOf[A]
          val chunkTail: ChunkedSeq[A] =
            if (c.len == 1) ChunkedSeq.empty
            else new ChunkedSeq.Chunk[A](c.arr, c.offset + 1, c.len - 1)
          val tail: ChunkedSeq[A] =
            if (chunkTail.isEmpty) rights
            else if (rights.isEmpty) chunkTail
            else new ChunkedSeq.Concat(chunkTail, rights, chunkTail.size + rights.size)
          result = (head, tail)
        } else if (current.isInstanceOf[ChunkedSeq.Concat[_]]) {
          val cc = current.asInstanceOf[ChunkedSeq.Concat[A]]
          rights =
            if (rights.isEmpty) cc.right
            else new ChunkedSeq.Concat(cc.right, rights, cc.right.size + rights.size)
          current = cc.left
        } else {
          // Empty node in a non-empty tree: defensive
          if (rights.nonEmpty) { current = rights; rights = ChunkedSeq.empty }
          else result = null.asInstanceOf[(A, ChunkedSeq[A])]
        }
      }
      if (result ne null) Some(result) else None
    }
  }

  /**
   * Init and last if non-empty. O(log N) amortized, stack-safe.
   */
  final def unsnoc: Option[(ChunkedSeq[A], A)] = {
    if (isEmpty) None
    else {
      var current: ChunkedSeq[A] = this
      var lefts: ChunkedSeq[A] = ChunkedSeq.empty
      var result: (ChunkedSeq[A], A) = null
      while (result eq null) {
        if (current.isInstanceOf[ChunkedSeq.Chunk[_]]) {
          val c = current.asInstanceOf[ChunkedSeq.Chunk[A]]
          val last = c.arr(c.offset + c.len - 1).asInstanceOf[A]
          val chunkInit: ChunkedSeq[A] =
            if (c.len == 1) ChunkedSeq.empty
            else new ChunkedSeq.Chunk[A](c.arr, c.offset, c.len - 1)
          val init: ChunkedSeq[A] =
            if (chunkInit.isEmpty) lefts
            else if (lefts.isEmpty) chunkInit
            else new ChunkedSeq.Concat(lefts, chunkInit, lefts.size + chunkInit.size)
          result = (init, last)
        } else if (current.isInstanceOf[ChunkedSeq.Concat[_]]) {
          val cc = current.asInstanceOf[ChunkedSeq.Concat[A]]
          lefts =
            if (lefts.isEmpty) cc.left
            else new ChunkedSeq.Concat(lefts, cc.left, lefts.size + cc.left.size)
          current = cc.right
        } else {
          if (lefts.nonEmpty) { current = lefts; lefts = ChunkedSeq.empty }
          else result = null.asInstanceOf[(ChunkedSeq[A], A)]
        }
      }
      if (result ne null) Some(result) else None
    }
  }

  /**
   * The first item if nonempty. O(log N), stack-safe.
   */
  final def headOption: Option[A] = {
    @tailrec def go(seq: ChunkedSeq[A]): Option[A] =
      if (seq.isInstanceOf[ChunkedSeq.Chunk[_]]) {
        val c = seq.asInstanceOf[ChunkedSeq.Chunk[A]]
        Some(c.arr(c.offset).asInstanceOf[A])
      } else if (seq.isInstanceOf[ChunkedSeq.Concat[_]])
        go(seq.asInstanceOf[ChunkedSeq.Concat[A]].left)
      else None
    go(this)
  }

  /**
   * All but the first item if nonempty. O(log N), stack-safe.
   */
  final def tailOption: Option[ChunkedSeq[A]] =
    uncons.map(_._2)

  /**
   * The last item if nonempty. O(log N), stack-safe.
   */
  final def lastOption: Option[A] = {
    @tailrec def go(seq: ChunkedSeq[A]): Option[A] =
      if (seq.isInstanceOf[ChunkedSeq.Chunk[_]]) {
        val c = seq.asInstanceOf[ChunkedSeq.Chunk[A]]
        Some(c.arr(c.offset + c.len - 1).asInstanceOf[A])
      } else if (seq.isInstanceOf[ChunkedSeq.Concat[_]])
        go(seq.asInstanceOf[ChunkedSeq.Concat[A]].right)
      else None
    go(this)
  }

  /**
   * Lookup the given index. O(log N) for balanced trees.
   */
  final def get(idx: Long): Option[A] =
    if (idx < 0L || idx >= size) None
    else Some(getUnsafe(idx))

  /**
   * Lookup the given index, throwing on out-of-bounds. O(log N) for balanced trees.
   */
  final def getUnsafe(idx0: Long): A = {
    if (idx0 < 0L || idx0 >= size)
      throw new NoSuchElementException(s"invalid index: $idx0")
    @tailrec def go(current: ChunkedSeq[A], remaining: Long): A =
      if (current.isInstanceOf[ChunkedSeq.Chunk[_]]) {
        val c = current.asInstanceOf[ChunkedSeq.Chunk[A]]
        c.arr(c.offset + remaining.toInt).asInstanceOf[A]
      } else if (current.isInstanceOf[ChunkedSeq.Concat[_]]) {
        val cc = current.asInstanceOf[ChunkedSeq.Concat[A]]
        if (remaining < cc.left.size) go(cc.left, remaining)
        else go(cc.right, remaining - cc.left.size)
      } else throw new NoSuchElementException(s"invalid index: $idx0")
    go(this, idx0)
  }

  /**
   * A strict, left-to-right fold. O(N), stack-safe.
   */
  final def foldLeft[B](init: B)(fn: (B, A) => B): B = {
    var acc = init
    var stack: List[ChunkedSeq[A]] = this :: Nil
    while (stack.nonEmpty) {
      val head = stack.head
      stack = stack.tail
      if (head.isInstanceOf[ChunkedSeq.Chunk[_]]) {
        val c = head.asInstanceOf[ChunkedSeq.Chunk[A]]
        var i = c.offset
        val end = c.offset + c.len
        while (i < end) {
          acc = fn(acc, c.arr(i).asInstanceOf[A])
          i += 1
        }
      } else if (head.isInstanceOf[ChunkedSeq.Concat[_]]) {
        val cc = head.asInstanceOf[ChunkedSeq.Concat[A]]
        stack = cc.left :: cc.right :: stack
      }
    }
    acc
  }

  /**
   * A strict, right-to-left fold. O(N), stack-safe.
   *
   * Note: cats.Foldable defines foldRight to work on Eval; we use a different name here not to collide with cats
   * syntax.
   */
  final def strictFoldRight[B](fin: B)(fn: (A, B) => B): B = {
    var acc = fin
    var stack: List[ChunkedSeq[A]] = this :: Nil
    while (stack.nonEmpty) {
      val head = stack.head
      stack = stack.tail
      if (head.isInstanceOf[ChunkedSeq.Chunk[_]]) {
        val c = head.asInstanceOf[ChunkedSeq.Chunk[A]]
        var i = c.offset + c.len - 1
        while (i >= c.offset) {
          acc = fn(c.arr(i).asInstanceOf[A], acc)
          i -= 1
        }
      } else if (head.isInstanceOf[ChunkedSeq.Concat[_]]) {
        val cc = head.asInstanceOf[ChunkedSeq.Concat[A]]
        // Process right first (popped first from stack), then left
        stack = cc.right :: cc.left :: stack
      }
    }
    acc
  }

  /**
   * Map to a type with a Monoid and combine in order. O(N), stack-safe.
   */
  final def foldMap[B](fn: A => B)(implicit B: Monoid[B]): B =
    foldLeft(B.empty)((b, a) => B.combine(b, fn(a)))

  /**
   * Standard map. O(N), stack-safe. Returns a balanced ChunkedSeq.
   */
  final def map[B](fn: A => B): ChunkedSeq[B] = {
    if (isEmpty) ChunkedSeq.empty
    else {
      val sz = size.toInt
      val arr = new Array[Any](sz)
      var idx = 0
      var stack: List[ChunkedSeq[A]] = this :: Nil
      while (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        if (head.isInstanceOf[ChunkedSeq.Chunk[_]]) {
          val c = head.asInstanceOf[ChunkedSeq.Chunk[A]]
          var i = c.offset
          val end = c.offset + c.len
          while (i < end) {
            arr(idx) = fn(c.arr(i).asInstanceOf[A])
            idx += 1
            i += 1
          }
        } else if (head.isInstanceOf[ChunkedSeq.Concat[_]]) {
          val cc = head.asInstanceOf[ChunkedSeq.Concat[A]]
          stack = cc.left :: cc.right :: stack
        }
      }
      ChunkedSeq.buildBalanced(arr, 0, arr.length)
    }
  }

  /**
   * Standard flatMap. O(result.size + this.size), stack-safe.
   */
  final def flatMap[B](fn: A => ChunkedSeq[B]): ChunkedSeq[B] = {
    @tailrec
    def loop(rev: List[A], acc: ChunkedSeq[B]): ChunkedSeq[B] =
      rev match {
        case Nil       => acc
        case h :: tail => loop(tail, fn(h) ++ acc)
      }
    loop(toListReverse, ChunkedSeq.empty)
  }

  /**
   * Keep only elements that match a predicate. O(N), stack-safe.
   */
  final def filter(fn: A => Boolean): ChunkedSeq[A] = {
    val it = toIterator
    var resList = List.empty[A]
    var changed = false
    while (it.hasNext) {
      val a = it.next()
      if (fn(a)) resList = a :: resList
      else changed = true
    }
    if (changed) ChunkedSeq.fromListReverse(resList)
    else this
  }

  /**
   * Same as filter(!fn(_)). O(N), stack-safe.
   */
  final def filterNot(fn: A => Boolean): ChunkedSeq[A] = {
    val it = toIterator
    var resList = List.empty[A]
    var changed = false
    while (it.hasNext) {
      val a = it.next()
      if (!fn(a)) resList = a :: resList
      else changed = true
    }
    if (changed) ChunkedSeq.fromListReverse(resList)
    else this
  }

  /**
   * Get an iterator through the ChunkedSeq. O(N) total, stack-safe.
   */
  final def toIterator: Iterator[A] = new ChunkedSeq.ChunkedSeqIterator(this)

  /**
   * Get a reverse iterator through the ChunkedSeq. O(N) total, stack-safe.
   */
  final def toReverseIterator: Iterator[A] = new ChunkedSeq.ChunkedSeqReverseIterator(this)

  /**
   * Convert to a scala standard List. O(N)
   */
  final def toList: List[A] = toIterator.toList

  /**
   * Convert to a scala standard list, but reversed. O(N)
   */
  final def toListReverse: List[A] = foldLeft(List.empty[A])((acc, a) => a :: acc)

  /**
   * We can efficiently drop things off the front. O(log N) for balanced trees, stack-safe.
   */
  final def drop(n: Long): ChunkedSeq[A] = {
    if (n <= 0L) this
    else if (n >= size) ChunkedSeq.empty
    else ChunkedSeq.dropImpl(this, n, ChunkedSeq.empty)
  }

  /**
   * Take the first n items. O(log N) for balanced trees, stack-safe.
   */
  final def take(n: Long): ChunkedSeq[A] = {
    if (n <= 0L) ChunkedSeq.empty
    else if (n >= size) this
    else ChunkedSeq.takeImpl(this, n, ChunkedSeq.empty)
  }

  /**
   * O(N) reversal.
   */
  final def reverse: ChunkedSeq[A] = {
    val it = toIterator
    var res: ChunkedSeq[A] = ChunkedSeq.empty
    while (it.hasNext) res = it.next() :: res
    res
  }

  /**
   * If the given index is in the sequence, update it, else return the current sequence. O(log N) for balanced trees.
   */
  final def updatedOrThis[A1 >: A](idx: Long, value: A1): ChunkedSeq[A1] = {
    if (idx < 0L || idx >= size) this
    else ChunkedSeq.updatedImpl(this, idx, value)
  }

  /**
   * If the given index is in the sequence, update and return Some(updated). Else return None. O(log N) for balanced
   * trees.
   */
  final def updated[A1 >: A](idx: Long, value: A1): Option[ChunkedSeq[A1]] = {
    val up = updatedOrThis(idx, value)
    if (up eq this) None else Some(up)
  }

  override def equals(that: Any): Boolean = that match {
    case cs: ChunkedSeq[_] =>
      (this eq cs) || (this.size == cs.size && {
        val li = this.toIterator
        val ri = cs.asInstanceOf[ChunkedSeq[Any]].toIterator
        while (li.hasNext) {
          if (li.next() != ri.next()) return false
        }
        true
      })
    case _ => false
  }

  override def hashCode(): Int =
    scala.util.hashing.MurmurHash3.orderedHash(toIterator)

  override def toString: String = {
    val strb = new java.lang.StringBuilder
    strb.append("ChunkedSeq(")
    val it = toIterator
    var first = true
    while (it.hasNext) {
      if (!first) strb.append(", "): Unit
      strb.append(it.next().toString)
      first = false
    }
    strb.append(")")
    strb.toString
  }

  // Testing utility: the depth of the internal tree. O(1)
  private[collections] def depth: Int
}

object ChunkedSeq extends ChunkedSeqInstances0 {

  /**
   * Maximum number of elements per array chunk.
   */
  private val ChunkSize = 32

  // ---- Internal node types ----

  private object EmptyNode extends ChunkedSeq[Nothing] {
    val size: Long = 0L
    val isEmpty: Boolean = true
    val nonEmpty: Boolean = false
    private[collections] val depth: Int = 0
  }

  final private[collections] class Chunk[A](
    val arr: Array[Any],
    val offset: Int,
    val len: Int
  ) extends ChunkedSeq[A] {
    val size: Long = len.toLong
    val isEmpty: Boolean = false
    val nonEmpty: Boolean = true
    private[collections] val depth: Int = 0
  }

  final private[collections] class Concat[A](
    val left: ChunkedSeq[A],
    val right: ChunkedSeq[A],
    val size: Long
  ) extends ChunkedSeq[A] {
    val isEmpty: Boolean = false
    val nonEmpty: Boolean = true
    private[collections] val depth: Int = math.max(left.depth, right.depth) + 1
  }

  // ---- Smart constructors ----

  def empty[A]: ChunkedSeq[A] = Empty

  val Empty: ChunkedSeq[Nothing] = EmptyNode

  private def one[A](a: A): ChunkedSeq[A] = {
    val arr = new Array[Any](1)
    arr(0) = a
    new Chunk[A](arr, 0, 1)
  }

  /**
   * Concatenate two ChunkedSeqs. O(1)
   */
  private[collections] def concatSafe[A](left: ChunkedSeq[A], right: ChunkedSeq[A]): ChunkedSeq[A] =
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else new Concat(left, right, left.size + right.size)

  // ---- Factory methods ----

  object NonEmpty {
    def apply[A](head: A, tail: ChunkedSeq[A]): ChunkedSeq[A] = head :: tail
    def unapply[A](fa: ChunkedSeq[A]): Option[(A, ChunkedSeq[A])] = fa.uncons
  }

  def fromList[A](list: List[A]): ChunkedSeq[A] = {
    if (list.isEmpty) empty
    else {
      val arr = new Array[Any](list.size)
      var i = 0
      var cur = list
      while (cur.nonEmpty) {
        arr(i) = cur.head
        cur = cur.tail
        i += 1
      }
      buildBalanced(arr, 0, arr.length)
    }
  }

  def fromListReverse[A](list: List[A]): ChunkedSeq[A] = {
    if (list.isEmpty) empty
    else {
      val arr = new Array[Any](list.size)
      var i = list.size - 1
      var cur = list
      while (cur.nonEmpty) {
        arr(i) = cur.head
        cur = cur.tail
        i -= 1
      }
      buildBalanced(arr, 0, arr.length)
    }
  }

  def fromSeq[A](s: scala.collection.Seq[A]): ChunkedSeq[A] = {
    if (s.isEmpty) empty
    else {
      val arr = new Array[Any](s.size)
      var i = 0
      s.foreach { a => arr(i) = a; i += 1 }
      buildBalanced(arr, 0, arr.length)
    }
  }

  private[collections] def buildBalanced[A](arr: Array[Any], from: Int, until: Int): ChunkedSeq[A] = {
    val len = until - from
    if (len <= 0) empty
    else if (len <= ChunkSize) new Chunk[A](arr, from, len)
    else {
      val mid = from + len / 2
      val left = buildBalanced[A](arr, from, mid)
      val right = buildBalanced[A](arr, mid, until)
      new Concat(left, right, len.toLong)
    }
  }

  // ---- Internal helpers ----

  @tailrec
  private def dropImpl[A](current: ChunkedSeq[A], n: Long, rights: ChunkedSeq[A]): ChunkedSeq[A] =
    if (current.isInstanceOf[Chunk[_]]) {
      val c = current.asInstanceOf[Chunk[A]]
      val dropped: ChunkedSeq[A] = new Chunk[A](c.arr, c.offset + n.toInt, c.len - n.toInt)
      if (rights.isEmpty) dropped
      else concatSafe(dropped, rights)
    } else if (current.isInstanceOf[Concat[_]]) {
      val cc = current.asInstanceOf[Concat[A]]
      if (n >= cc.left.size)
        dropImpl(cc.right, n - cc.left.size, rights)
      else
        dropImpl(cc.left, n, if (rights.isEmpty) cc.right else concatSafe(cc.right, rights))
    } else rights

  @tailrec
  private def takeImpl[A](current: ChunkedSeq[A], n: Long, lefts: ChunkedSeq[A]): ChunkedSeq[A] =
    if (current.isInstanceOf[Chunk[_]]) {
      val c = current.asInstanceOf[Chunk[A]]
      val taken: ChunkedSeq[A] = new Chunk[A](c.arr, c.offset, n.toInt)
      if (lefts.isEmpty) taken
      else concatSafe(lefts, taken)
    } else if (current.isInstanceOf[Concat[_]]) {
      val cc = current.asInstanceOf[Concat[A]]
      if (n <= cc.left.size)
        takeImpl(cc.left, n, lefts)
      else
        takeImpl(cc.right, n - cc.left.size, if (lefts.isEmpty) cc.left else concatSafe(lefts, cc.left))
    } else lefts

  private def updatedImpl[A](seq: ChunkedSeq[A], idx: Long, value: A): ChunkedSeq[A] =
    if (seq.isInstanceOf[Chunk[_]]) {
      val c = seq.asInstanceOf[Chunk[A]]
      val newArr = new Array[Any](c.len)
      System.arraycopy(c.arr, c.offset, newArr, 0, c.len)
      newArr(idx.toInt) = value
      new Chunk[A](newArr, 0, c.len)
    } else if (seq.isInstanceOf[Concat[_]]) {
      val cc = seq.asInstanceOf[Concat[A]]
      if (idx < cc.left.size) new Concat(updatedImpl(cc.left, idx, value), cc.right, cc.size)
      else new Concat(cc.left, updatedImpl(cc.right, idx - cc.left.size, value), cc.size)
    } else seq

  // ---- Iterators ----

  final private[collections] class ChunkedSeqIterator[A](root: ChunkedSeq[A]) extends Iterator[A] {
    private[this] var stack: List[ChunkedSeq[A]] = if (root.nonEmpty) root :: Nil else Nil
    private[this] var currentArr: Array[Any] = _
    private[this] var currentIdx: Int = 0
    private[this] var currentEnd: Int = 0

    advance()

    private[this] def advance(): Unit = {
      while (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        if (head.isInstanceOf[Chunk[_]]) {
          val c = head.asInstanceOf[Chunk[A]]
          currentArr = c.arr
          currentIdx = c.offset
          currentEnd = c.offset + c.len
          return
        } else if (head.isInstanceOf[Concat[_]]) {
          val cc = head.asInstanceOf[Concat[A]]
          stack = cc.left :: cc.right :: stack
        }
        // else Empty, skip
      }
      currentArr = null
    }

    def hasNext: Boolean = currentArr ne null

    def next(): A = {
      if (currentArr eq null) throw new NoSuchElementException("ChunkedSeq.toIterator exhausted")
      val result = currentArr(currentIdx).asInstanceOf[A]
      currentIdx += 1
      if (currentIdx >= currentEnd) advance()
      result
    }
  }

  final private[collections] class ChunkedSeqReverseIterator[A](root: ChunkedSeq[A]) extends Iterator[A] {
    private[this] var stack: List[ChunkedSeq[A]] = if (root.nonEmpty) root :: Nil else Nil
    private[this] var currentArr: Array[Any] = _
    private[this] var currentIdx: Int = 0
    private[this] var currentEnd: Int = 0

    advance()

    private[this] def advance(): Unit = {
      while (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        if (head.isInstanceOf[Chunk[_]]) {
          val c = head.asInstanceOf[Chunk[A]]
          currentArr = c.arr
          currentIdx = c.offset + c.len - 1
          currentEnd = c.offset
          return
        } else if (head.isInstanceOf[Concat[_]]) {
          val cc = head.asInstanceOf[Concat[A]]
          // Right first for reverse order
          stack = cc.right :: cc.left :: stack
        }
      }
      currentArr = null
    }

    def hasNext: Boolean = currentArr ne null

    def next(): A = {
      if (currentArr eq null) throw new NoSuchElementException("ChunkedSeq.toReverseIterator exhausted")
      val result = currentArr(currentIdx).asInstanceOf[A]
      currentIdx -= 1
      if (currentIdx < currentEnd) advance()
      result
    }
  }

  // ---- Typeclass instances ----

  implicit def catsCollectionChunkedSeqOrder[A: Order]: Order[ChunkedSeq[A]] =
    new Order[ChunkedSeq[A]] {
      val ordA: Order[A] = Order[A]
      def compare(l: ChunkedSeq[A], r: ChunkedSeq[A]): Int = {
        val li = l.toIterator
        val ri = r.toIterator
        while (li.hasNext && ri.hasNext) {
          val c = ordA.compare(li.next(), ri.next())
          if (c != 0) return c
        }
        if (li.hasNext) 1
        else if (ri.hasNext) -1
        else 0
      }
    }

  private[collections] def eqChunkedSeq[A: Eq]: Eq[ChunkedSeq[A]] = {
    val eqA = Eq[A]
    new Eq[ChunkedSeq[A]] {
      def eqv(l: ChunkedSeq[A], r: ChunkedSeq[A]): Boolean = {
        if (l.size != r.size) false
        else {
          val li = l.toIterator
          val ri = r.toIterator
          while (li.hasNext) {
            if (!eqA.eqv(li.next(), ri.next())) return false
          }
          true
        }
      }
    }
  }

  implicit def catsCollectionChunkedSeqMonoid[A]: Monoid[ChunkedSeq[A]] =
    new Monoid[ChunkedSeq[A]] {
      def empty: ChunkedSeq[A] = ChunkedSeq.empty
      def combine(l: ChunkedSeq[A], r: ChunkedSeq[A]): ChunkedSeq[A] = l ++ r
    }

  implicit def catsCollectionChunkedSeqShow[A: Show]: Show[ChunkedSeq[A]] =
    Show.show[ChunkedSeq[A]] { cs =>
      val sa = Show[A]
      cs.toIterator.map(sa.show(_)).mkString("ChunkedSeq(", ", ", ")")
    }

  implicit val catsCollectionChunkedSeqInstances: Traverse[ChunkedSeq]
    with Alternative[ChunkedSeq]
    with Monad[ChunkedSeq]
    with CoflatMap[ChunkedSeq]
    with FunctorFilter[ChunkedSeq] =
    new Traverse[ChunkedSeq]
      with Alternative[ChunkedSeq]
      with Monad[ChunkedSeq]
      with CoflatMap[ChunkedSeq]
      with FunctorFilter[ChunkedSeq] {

      def coflatMap[A, B](fa: ChunkedSeq[A])(fn: ChunkedSeq[A] => B): ChunkedSeq[B] = {
        @tailrec
        def loop(fa: ChunkedSeq[A], revList: List[B]): ChunkedSeq[B] =
          fa match {
            case NonEmpty(_, tail) => loop(tail, fn(fa) :: revList)
            case _                 => fromListReverse(revList)
          }
        loop(fa, Nil)
      }

      def combineK[A](l: ChunkedSeq[A], r: ChunkedSeq[A]): ChunkedSeq[A] = l ++ r

      def empty[A]: ChunkedSeq[A] = ChunkedSeq.empty

      override def exists[A](fa: ChunkedSeq[A])(fn: A => Boolean): Boolean =
        fa.toIterator.exists(fn)

      override def flatMap[A, B](fa: ChunkedSeq[A])(fn: A => ChunkedSeq[B]): ChunkedSeq[B] =
        fa.flatMap(fn)

      def foldLeft[A, B](fa: ChunkedSeq[A], init: B)(fn: (B, A) => B): B =
        fa.foldLeft(init)(fn)

      override def foldMap[A, B: Monoid](fa: ChunkedSeq[A])(fn: A => B): B =
        fa.foldMap(fn)

      def foldRight[A, B](fa: ChunkedSeq[A], fin: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(stack: List[ChunkedSeq[A]]): Eval[B] =
          stack match {
            case Nil          => fin
            case head :: tail =>
              if (head.isInstanceOf[Chunk[_]]) {
                val c = head.asInstanceOf[Chunk[A]]
                def elemLoop(i: Int): Eval[B] =
                  if (i >= c.offset + c.len) Eval.defer(loop(tail))
                  else fn(c.arr(i).asInstanceOf[A], Eval.defer(elemLoop(i + 1)))
                elemLoop(c.offset)
              } else if (head.isInstanceOf[Concat[_]]) {
                val cc = head.asInstanceOf[Concat[A]]
                Eval.defer(loop(cc.left :: cc.right :: tail))
              } else Eval.defer(loop(tail))
          }
        loop(if (fa.nonEmpty) fa :: Nil else Nil)
      }

      override def forall[A](fa: ChunkedSeq[A])(fn: A => Boolean): Boolean =
        fa.toIterator.forall(fn)

      def functor: Functor[ChunkedSeq] = this

      def mapFilter[A, B](ta: ChunkedSeq[A])(fn: A => Option[B]): ChunkedSeq[B] = {
        val it = ta.toIterator
        var resList = List.empty[B]
        while (it.hasNext) {
          fn(it.next()) match {
            case Some(b) => resList = b :: resList
            case None    => ()
          }
        }
        fromListReverse(resList)
      }

      override def filter[A](ta: ChunkedSeq[A])(fn: A => Boolean): ChunkedSeq[A] =
        ta.filter(fn)

      override def get[A](fa: ChunkedSeq[A])(idx: Long): Option[A] =
        fa.get(idx)

      override def isEmpty[A](fa: ChunkedSeq[A]): Boolean = fa.isEmpty

      override def map[A, B](fa: ChunkedSeq[A])(fn: A => B): ChunkedSeq[B] =
        fa.map(fn)

      override def nonEmpty[A](fa: ChunkedSeq[A]): Boolean = fa.nonEmpty

      def pure[A](a: A): ChunkedSeq[A] = one(a)

      override def reduceLeftToOption[A, B](fa: ChunkedSeq[A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.uncons match {
          case None            => None
          case Some((a, tail)) =>
            Some {
              if (tail.isEmpty) f(a)
              else tail.foldLeft(f(a))(g)
            }
        }

      override def reduceRightToOption[A, B](
        fa: ChunkedSeq[A]
      )(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        fa.uncons match {
          case None            => Eval.now(None)
          case Some((a, tail)) =>
            if (tail.isEmpty) Eval.now(Some(f(a)))
            else foldRight(tail, Eval.now(f(a)))(g).map(Some(_))
        }

      override def toList[A](fa: ChunkedSeq[A]): List[A] =
        fa.toList

      def tailRecM[A, B](a: A)(fn: A => ChunkedSeq[Either[A, B]]): ChunkedSeq[B] = {
        @tailrec
        def loop(stack: List[ChunkedSeq[Either[A, B]]], acc: List[B]): List[B] =
          stack match {
            case head :: tail =>
              head match {
                case NonEmpty(either, rest) =>
                  either match {
                    case Right(b) => loop(rest :: tail, b :: acc)
                    case Left(aa) => loop(fn(aa) :: rest :: tail, acc)
                  }
                case _ => loop(tail, acc)
              }
            case Nil => acc
          }
        val res = loop(fn(a) :: Nil, Nil)
        fromListReverse(res)
      }

      def traverse[G[_], A, B](fa: ChunkedSeq[A])(f: A => G[B])(implicit G: Applicative[G]): G[ChunkedSeq[B]] = {
        // Tree-aware traversal: recursion depth is O(log N + ChunkSize), stack-safe for balanced trees
        def traverseChunk(c: Chunk[A]): G[ChunkedSeq[B]] = {
          var i = c.offset + c.len - 1
          var acc: G[List[B]] = G.pure(List.empty[B])
          while (i >= c.offset) {
            val a = c.arr(i).asInstanceOf[A]
            acc = G.map2(f(a), acc)(_ :: _)
            i -= 1
          }
          G.map(acc)(fromList(_))
        }

        def go(seq: ChunkedSeq[A]): G[ChunkedSeq[B]] =
          if (seq.isEmpty) G.pure(ChunkedSeq.empty[B])
          else if (seq.isInstanceOf[Chunk[_]])
            traverseChunk(seq.asInstanceOf[Chunk[A]])
          else if (seq.isInstanceOf[Concat[_]]) {
            val cc = seq.asInstanceOf[Concat[A]]
            G.map2(go(cc.left), go(cc.right))(concatSafe(_, _))
          } else G.pure(ChunkedSeq.empty[B])

        go(fa)
      }
    }
}

abstract private[collections] class ChunkedSeqInstances0 extends ChunkedSeqInstances1 {
  implicit def catsCollectionChunkedSeqPartialOrder[A: PartialOrder]: PartialOrder[ChunkedSeq[A]] =
    new PartialOrder[ChunkedSeq[A]] {
      val ordA: PartialOrder[A] = PartialOrder[A]
      def partialCompare(l: ChunkedSeq[A], r: ChunkedSeq[A]): Double = {
        val li = l.toIterator
        val ri = r.toIterator
        while (li.hasNext && ri.hasNext) {
          val c = ordA.partialCompare(li.next(), ri.next())
          if (c != 0.0) return c
        }
        if (li.hasNext) 1.0
        else if (ri.hasNext) -1.0
        else 0.0
      }
    }
}

abstract private[collections] class ChunkedSeqInstances1 {
  implicit def catsCollectionChunkedSeqEq[A: Eq]: Eq[ChunkedSeq[A]] =
    ChunkedSeq.eqChunkedSeq
}
