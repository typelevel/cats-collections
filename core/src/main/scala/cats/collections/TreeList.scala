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
  Semigroup,
  Show,
  Traverse
}

import cats.implicits._
import scala.annotation.tailrec

/**
 * Implementation of "Purely Functional Random Access Lists" by Chris Okasaki. This gives O(1) cons and uncons, and 2
 * log_2 N lookup.
 *
 * A consequence of the log N complexity is that naive recursion on the inner methods will (almost) never blow the stack
 * since the depth of the structure is log N, this greatly simplifies many methods. A key example is that unlike List,
 * using a TreeList you can sequence and traverse very large lists without blowing the stack since the stack depth is
 * only log N.
 *
 * This data-structure is useful when you want fast cons and uncons, but also want to index. It does not have an
 * optimized concatenation.
 */
sealed abstract class TreeList[+A] {

  /**
   * This is like headOption and tailOption in one call. O(1)
   */
  def uncons: Option[(A, TreeList[A])]

  /**
   * The first item if nonempty
   */
  def headOption: Option[A]

  /**
   * All but the first item if nonempty
   */
  def tailOption: Option[TreeList[A]]

  /**
   * put an item on the front. O(1)
   */
  def prepend[A1 >: A](a1: A1): TreeList[A1]

  /**
   * lookup the given index in the list. O(log N). if the item is < 0 or >= size, return None
   */
  def get(idx: Long): Option[A]

  /**
   * lookup the given index in the list. O(log N). if the item is < 0 or >= size, else throw
   */
  def getUnsafe(idx: Long): A

  /**
   * get the last element, if it is not empty. O(log N) a bit more efficient than get(size - 1)
   */
  def lastOption: Option[A]

  /**
   * How many items are in this TreeList. O(log N)
   */
  def size: Long

  /**
   * A strict, left-to-right fold: O(N)
   */
  def foldLeft[B](init: B)(fn: (B, A) => B): B

  /**
   * a strict, right-to-left fold. Note, cats.Foldable defines foldRight to work on Eval, we use a different name here
   * not to collide with the cats syntax
   *
   * O(N)
   */
  def strictFoldRight[B](fin: B)(fn: (A, B) => B): B

  /**
   * standard map. O(N) operation. Since this preserves structure, it is more efficient than converting toIterator,
   * mapping the iterator, and converting back
   */
  def map[B](fn: A => B): TreeList[B]

  /**
   * We can efficiently drop things off the front without rebuilding
   *
   * O(n) operation (complexity is the number of things being dropped)
   */
  def drop(n: Long): TreeList[A]

  /**
   * Get an iterator through the TreeList
   *
   * We can iterate through in O(N) time
   */
  def toIterator: Iterator[A]

  /**
   * Get a reverse iterator through the TreeList not as efficient as going in the left to right order.
   *
   * It appears this is N log N in cost (although possible only N, as we have not proven the bound on cost)
   *
   * This is useful if you only want a few things from the right, if you need to iterate the entire list, it is better
   * to to use toListReverse which is O(N)
   *
   * This is only a constant more efficient that iterating via random access using get
   */
  def toReverseIterator: Iterator[A]

  /**
   * If the given index is in the list, update it, else return the current list with no change.
   *
   * O(log N)
   */
  def updatedOrThis[A1 >: A](idx: Long, value: A1): TreeList[A1]

  /**
   * map to a type with a Monoid and combine in the order of the list, O(N)
   */
  def foldMap[B: Monoid](fn: A => B): B

  /**
   * returns true if size == 0. O(1)
   */
  def isEmpty: Boolean

  /**
   * returns true if size != 0. O(1)
   */
  def nonEmpty: Boolean

  /**
   * Convert to a scala standard List. O(N)
   */
  def toList: List[A]

  /**
   * Convert to a scala standard list, but reversed O(N)
   */
  def toListReverse: List[A]

  /**
   * return the right most full binary tree on the right, the rest on left val (l, r) = items.split assert((l ++ r) ==
   * items)
   *
   * O(log N)
   */
  def split: (TreeList[A], TreeList[A])

  /*
   * The following methods do not have an optimized
   * implementation and are expressed in terms of the above
   * O(1)
   */
  final def ::[A1 >: A](a1: A1): TreeList[A1] = prepend(a1)

  // This is a test method to ensure the invariant on depth is correct
  // O(log N)
  private[collections] def maxDepth: Int

  override def toString: String = {
    val strb = new java.lang.StringBuilder
    strb.append("TreeList(")
    @tailrec
    def loop(first: Boolean, l: TreeList[A]): Unit =
      l.uncons match {
        case None => ()
        case Some(h, t) =>
          if (!first) strb.append(", ")
          strb.append(h.toString)
          loop(false, t)
      }

    loop(true, this)
    strb.append(")")
    strb.toString
  }

  /**
   * Concatenate two TreeLists. This requires doing as much work as this.size
   *
   * O(this.size)
   */
  final def ++[A1 >: A](that: TreeList[A1]): TreeList[A1] = {
    @tailrec
    def loop(ls: List[A], that: TreeList[A1]): TreeList[A1] =
      ls match {
        case Nil       => that
        case h :: tail => loop(tail, h :: that)
      }
    if (that.nonEmpty) loop(toListReverse, that)
    else this
  }

  /**
   * keep the elements that match a predicate O(N)
   */
  final def filter(fn: A => Boolean): TreeList[A] = {
    val as = toIterator
    var resList = List.empty[A]
    var changed = false
    while (as.hasNext) {
      val a = as.next()
      if (fn(a)) {
        resList = a :: resList
      } else { changed = true }
    }
    if (changed) TreeList.fromListReverse(resList)
    else this
  }

  /**
   * same as filter(!fn(_)) O(N)
   */
  final def filterNot(fn: A => Boolean): TreeList[A] = {
    // we reimplement this to avoid creating an extra
    // closure since scala can't optimize function
    // composition well
    val as = toIterator
    var resList = List.empty[A]
    var changed = false
    while (as.hasNext) {
      val a = as.next()
      if (!fn(a)) {
        resList = a :: resList
      } else { changed = true }
    }
    if (changed) TreeList.fromListReverse(resList)
    else this
  }

  /**
   * Standard flatMap on a List type. O(result.size + this.size)
   */
  final def flatMap[B](fn: A => TreeList[B]): TreeList[B] = {
    @tailrec
    def loop(rev: List[A], acc: TreeList[B]): TreeList[B] =
      rev match {
        case Nil => acc
        case h :: tail =>
          loop(tail, fn(h) ++ acc)
      }
    loop(toListReverse, TreeList.Empty)
  }

  /**
   * O(N) reversal of the treeList
   */
  final def reverse: TreeList[A] = {
    val revTrees = toIterator
    var res: TreeList[A] = TreeList.Empty
    while (revTrees.hasNext) {
      res = revTrees.next() :: res
    }
    res
  }

  /**
   * Take the first n things off the list. O(n)
   */
  final def take(n: Long): TreeList[A] = {
    val takeIt = toIterator
    var cnt = 0L
    var res = List.empty[A]
    while (takeIt.hasNext && cnt < n) {
      res = takeIt.next() :: res
      cnt += 1L
    }
    TreeList.fromListReverse(res)
  }

  /**
   * If the given index is in the list, update and return Some(updated). else return None
   *
   * O(log N)
   */
  final def updated[A1 >: A](idx: Long, value: A1): Option[TreeList[A1]] = {
    val up = updatedOrThis(idx, value)
    if (up eq this) None else Some(up)
  }
}

object TreeList extends TreeListInstances0 {
  private object Impl {
    sealed trait Nat {
      def value: Int
    }
    object Nat {
      case class Succ[P <: Nat](prev: P) extends Nat {
        val value: Int = prev.value + 1
      }
      case object Zero extends Nat {
        def value: Int = 0
      }

      /*
       * We don't need to make this too large,
       * it is the smallest items that are changing
       * the most, at big depths, we don't win
       * much by this memoization, and by making
       * sure we exercise all branches we have better
       * tested code
       */
      private[this] val memoUpTo: Int = 12
      private[this] val memoNat: Array[Nat] = {
        @tailrec
        def build(n: Nat, acc: List[Nat], cnt: Int): Array[Nat] =
          if (cnt >= memoUpTo) acc.reverse.toArray
          else {
            val s = Succ(n)
            build(s, s :: acc, cnt + 1)
          }

        build(Zero, Nil, 0)
      }

      /**
       * This is a memoized Succ constructor since we generally only build a small number of Succ instances
       */
      def succ[N <: Nat](n: N): Succ[N] = {
        val v = n.value
        if (v < memoUpTo) memoNat(v).asInstanceOf[Succ[N]]
        else Succ(n)
      }
    }
    sealed abstract class NatEq[A <: Nat, B <: Nat] {
      def subst[F[_ <: Nat]](f: F[A]): F[B]
    }

    object NatEq {
      implicit def refl[A <: Nat]: NatEq[A, A] =
        new NatEq[A, A] {
          def subst[F[_ <: Nat]](f: F[A]): F[A] = f
        }

      // Cache this so we avoid allocating repeatedly
      private[this] val someRefl: Option[NatEq[Nat.Zero.type, Nat.Zero.type]] =
        Some(refl[Nat.Zero.type])

      def maybeEq[N1 <: Nat, N2 <: Nat](n1: N1, n2: N2): Option[NatEq[N1, N2]] =
        // I don't see how to prove this in scala, but it is true
        if (n1.value == n2.value) someRefl.asInstanceOf[Option[NatEq[N1, N2]]]
        else None
    }
    sealed abstract class Tree[+N <: Nat, +A] {
      def value: A
      def depth: N
      def size: Long // this is 2^(depth + 1) - 1
      def get(idx: Long): Option[A]
      def getUnsafe(idx: Long): A
      def map[B](fn: A => B): Tree[N, B]
      def foldRight[B](fin: B)(fn: (A, B) => B): B
      def foldMap[B: Semigroup](fn: A => B): B
      def updated[A1 >: A](idx: Long, a: A1): Tree[N, A1]
    }
    case class Root[A](value: A) extends Tree[Nat.Zero.type, A] {
      def depth: Nat.Zero.type = Nat.Zero
      def size: Long = 1L
      def get(idx: Long): Option[A] =
        if (idx == 0L) Some(value) else None
      def getUnsafe(idx: Long): A =
        if (idx == 0L) value
        else throw new NoSuchElementException("invalid index")

      def map[B](fn: A => B): Tree[Nat.Zero.type, B] = Root(fn(value))
      def foldRight[B](fin: B)(fn: (A, B) => B): B = fn(value, fin)
      def foldMap[B: Semigroup](fn: A => B): B = fn(value)
      def updated[A1 >: A](idx: Long, a: A1): Tree[Nat.Zero.type, A1] =
        // we could check that idx == 0L here, but we have tests so no need to branch
        // and have a false branch that is never taken
        Root(a)
    }
    case class Balanced[N <: Nat, A](value: A, left: Tree[N, A], right: Tree[N, A]) extends Tree[Nat.Succ[N], A] {
      // this should be a val, so we save the result and not do O(log N) work to compute it
      // prefer accessing left even though right is the same size, since left
      // is more likely to be accessed being at the front, we assume it should
      // have better cache performance
      val depth: Nat.Succ[N] = Nat.succ(left.depth)
      val size: Long = 1L + (left.size << 1) // 2n + 1, since we have a balanced tree
      def get(idx: Long): Option[A] =
        if (idx == 0L) Some(value)
        else if (idx <= left.size) left.get(idx - 1L)
        else right.get(idx - (left.size + 1L))

      def getUnsafe(idx: Long): A =
        if (idx == 0L) value
        else if (idx <= left.size) left.getUnsafe(idx - 1L)
        else right.getUnsafe(idx - (left.size + 1L))

      def map[B](fn: A => B): Tree[Nat.Succ[N], B] =
        Balanced[N, B](fn(value), left.map(fn), right.map(fn))

      def foldRight[B](fin: B)(fn: (A, B) => B): B = {
        val rightB = right.foldRight(fin)(fn)
        val leftB = left.foldRight(rightB)(fn)
        fn(value, leftB)
      }
      def foldMap[B: Semigroup](fn: A => B): B = {
        val sg = Semigroup[B]
        sg.combine(fn(value), sg.combine(left.foldMap(fn), right.foldMap(fn)))
      }

      def updated[A1 >: A](idx: Long, a: A1): Tree[Nat.Succ[N], A1] =
        if (idx == 0L) Balanced(a, left, right)
        else if (idx <= left.size) copy(left = left.updated(idx - 1L, a))
        else copy(right = right.updated(idx - (left.size + 1L), a))
    }

    def traverseTree[F[_]: Applicative, A, B, N <: Nat](ta: Tree[N, A], fn: A => F[B]): F[Tree[N, B]] =
      ta match {
        case Root(a) => fn(a).map(Root(_))
        case Balanced(a, left, right) =>
          (fn(a), traverseTree(left, fn), traverseTree(right, fn)).mapN { (b, l, r) =>
            Balanced(b, l, r)
          }
      }

    implicit def eqTree[A: Eq]: Eq[Tree[Nat, A]] =
      new Eq[Tree[Nat, A]] {
        val eqA: Eq[A] = Eq[A]
        def eqv(l: Tree[Nat, A], r: Tree[Nat, A]): Boolean =
          (l, r) match {
            case (Root(a), Root(b)) => eqA.eqv(a, b)
            case (Balanced(a, al, ar), Balanced(b, bl, br)) =>
              eqA.eqv(a, b) && eqv(al, bl) && eqv(ar, br)
            case _ => false
          }
      }

    final class TreeListIterator[A](from: TreeList[A]) extends Iterator[A] {
      private var nexts: List[Tree[Nat, A]] =
        toListOfTrees(from)

      def hasNext: Boolean = nexts.nonEmpty
      def next(): A =
        if (nexts.isEmpty) throw new NoSuchElementException("TreeList.toIterator exhausted")
        else {
          nexts.head match {
            case Root(a) =>
              nexts = nexts.tail
              a
            case Balanced(a, l, r) =>
              nexts = l :: r :: nexts.tail
              a
          }
        }
    }

    final class TreeListReverseIterator[A](from: TreeList[A]) extends Iterator[A] {
      private var nexts: List[Tree[Nat, A]] = from match {
        case Trees(treeList) => treeList.reverse
      }

      def hasNext: Boolean = nexts.nonEmpty

      /*
       * The cost to call next when left most item has depth D is
       * D. We know that D <= log_2 N, so in the worst case iteration
       * is N log N. A tree has most items at the deepest levels,
       * to this implies that we do need O(log N) work in the average
       * case to access a next item
       *
       * On the other hand, we tear down depth as we do this and save
       * that state for the future, so it could be that the total work
       * is still O(N). This is an open question.
       */
      @tailrec
      final def next(): A =
        if (nexts.isEmpty) throw new NoSuchElementException("TreeList.toReverseIterator exhausted")
        else
          nexts.head match {
            case Root(a) =>
              nexts = nexts.tail
              a
            case Balanced(a, l, r) =>
              // switch the order
              nexts = r :: l :: Root(a) :: nexts.tail
              next()
          }
    }
  }

  import Impl._

  final private case class Trees[A](treeList: List[Tree[Nat, A]]) extends TreeList[A] {
    def prepend[A1 >: A](a1: A1): TreeList[A1] =
      treeList match {
        case h1 :: h2 :: rest =>
          // we introduce this method to be able to name the types on h1 and h2
          // since we need to work with them in a few places
          def go[N1 <: Nat, N2 <: Nat, A2 <: A](t1: Tree[N1, A2], t2: Tree[N2, A2]): TreeList[A1] =
            NatEq.maybeEq[N1, N2](t1.depth, t2.depth) match {
              case Some(eqv) =>
                type T[N <: Nat] = Tree[N, A2]
                Trees(Balanced[N2, A1](a1, eqv.subst[T](t1), t2) :: rest)
              case None =>
                Trees(Root(a1) :: treeList)
            }
          go(h1, h2)
        case lessThan2 => Trees(Root(a1) :: lessThan2)
      }

    @inline private[this] def tailTreeList(head: Tree[Nat, A]): List[Tree[Nat, A]] =
      // benchmarks show this to be faster, ugly, but faster
      if (head.isInstanceOf[Root[_]]) {
        treeList.tail
      } else {
        val balanced = head.asInstanceOf[Balanced[Nat, A]]
        balanced.left :: balanced.right :: treeList.tail
      }

    def uncons: Option[(A, TreeList[A])] =
      if (treeList.nonEmpty) {
        val h = treeList.head
        Some((h.value, Trees(tailTreeList(h))))
      } else None

    def headOption: Option[A] =
      if (treeList.isEmpty) None
      else Some(treeList.head.value)

    def tailOption: Option[TreeList[A]] =
      if (treeList.isEmpty) None
      else {
        val tl1 = tailTreeList(treeList.head)
        Some(Trees(tl1))
      }

    def get(idx: Long): Option[A] = {
      @tailrec
      def loop(idx: Long, treeList: List[Tree[Nat, A]]): Option[A] =
        if (treeList.nonEmpty) {
          val h = treeList.head
          if (h.size <= idx) loop(idx - h.size, treeList.tail)
          else h.get(idx)
        } else None

      loop(idx, treeList)
    }

    def getUnsafe(idx0: Long): A = {
      @tailrec
      def loop(idx: Long, treeList: List[Tree[Nat, A]]): A = {
        if (treeList.nonEmpty) {
          val h = treeList.head
          if (h.size <= idx) loop(idx - h.size, treeList.tail)
          else h.getUnsafe(idx)
        } else throw new NoSuchElementException(s"invalid index: $idx0")
      }

      loop(idx0, treeList)
    }

    def lastOption: Option[A] = {
      @tailrec
      def loop(treeList: List[Tree[Nat, A]]): Option[A] =
        treeList match {
          case head :: tail =>
            if (tail.isEmpty)
              head match {
                case Root(a)           => Some(a)
                case Balanced(_, _, r) => loop(r :: Nil)
              }
            else loop(tail)
          case Nil => None
        }
      loop(treeList)
    }

    def size: Long = {
      @tailrec
      def loop(treeList: List[Tree[Nat, A]], acc: Long): Long =
        if (treeList.nonEmpty) loop(treeList.tail, acc + treeList.head.size)
        else acc
      loop(treeList, 0L)
    }

    def foldLeft[B](init: B)(fn: (B, A) => B): B = {
      @tailrec
      def loop(init: B, rest: List[Tree[Nat, A]]): B =
        if (rest.nonEmpty) {
          rest.head match {
            case Root(a)           => loop(fn(init, a), rest.tail)
            case Balanced(a, l, r) => loop(fn(init, a), l :: r :: rest.tail)
          }
        } else init

      loop(init, treeList)
    }

    def strictFoldRight[B](fin: B)(fn: (A, B) => B): B =
      treeList.reverse.foldLeft(fin) { (b, treea) =>
        treea.foldRight(b)(fn)
      }

    def isEmpty: Boolean = treeList.isEmpty
    def nonEmpty: Boolean = treeList.nonEmpty

    def map[B](fn: A => B): TreeList[B] = Trees(treeList.map(_.map(fn)))

    def drop(n: Long): TreeList[A] = {
      @tailrec
      def loop(n: Long, treeList: List[Tree[Nat, A]]): TreeList[A] =
        treeList match {
          case Nil          => empty
          case _ if n <= 0L => Trees(treeList)
          case h :: tail =>
            if (h.size <= n) loop(n - h.size, tail)
            else {
              h match {
                case Balanced(_, l, r) =>
                  if (n > l.size + 1L) loop(n - l.size - 1L, r :: tail)
                  else if (n > 1L) loop(n - 1L, l :: r :: tail)
                  else Trees(l :: r :: tail)
                case Root(_) =>
                  // $COVERAGE-OFF$
                  sys.error(s"unreachable, $h has size == 1 which is <= n ($n)")
                // $COVERAGE-ON$
              }
            }
        }

      loop(n, treeList)
    }

    def split: (TreeList[A], TreeList[A]) =
      treeList match {
        case Nil            => (empty, empty)
        case Root(_) :: Nil => (this, empty)
        case Balanced(a, l, r) :: Nil =>
          (Trees(Root(a) :: l :: Nil), Trees(r :: Nil))
        case moreThanOne =>
          (Trees(moreThanOne.init), Trees(moreThanOne.last :: Nil))
      }

    def toIterator: Iterator[A] = new TreeListIterator(this)
    def toReverseIterator: Iterator[A] = new TreeListReverseIterator(this)

    def updatedOrThis[A1 >: A](idx: Long, a: A1): TreeList[A1] = {
      @tailrec
      def loop(idx: Long, treeList: List[Tree[Nat, A1]], front: List[Tree[Nat, A1]]): TreeList[A1] =
        if (treeList.nonEmpty && idx >= 0) {
          val h = treeList.head
          val tail = treeList.tail
          if (h.size <= idx) loop(idx - h.size, tail, h :: front)
          else {
            val h1 = h.updated(idx, a)
            // now rebuild the front of the list
            Trees(front reverse_::: (h1 :: tail))
          }
        } else this

      loop(idx, treeList, Nil)
    }

    def foldMap[B: Monoid](fn: A => B): B =
      Monoid[B].combineAll(treeList.map(_.foldMap(fn)))

    override def toList: List[A] = {
      val builder = List.newBuilder[A]
      @tailrec
      def loop(treeList: List[Tree[Nat, A]]): Unit =
        treeList match {
          case Root(a) :: tail =>
            builder += a
            loop(tail)
          case Balanced(a, l, r) :: tail =>
            builder += a
            loop(l :: r :: tail)
          case Nil => ()
        }
      loop(treeList)
      builder.result()
    }

    override def toListReverse: List[A] = {
      @tailrec
      def loop(treeList: List[Tree[Nat, A]], acc: List[A]): List[A] =
        treeList match {
          case Root(a) :: tail =>
            loop(tail, a :: acc)
          case Balanced(a, l, r) :: tail =>
            loop(l :: r :: tail, a :: acc)
          case Nil => acc
        }
      loop(treeList, Nil)
    }

    private[collections] def maxDepth: Int = {
      val listLength = treeList.size
      val treeDepth = treeList.map(_.depth.value) match {
        case Nil  => 0
        case nonE => nonE.max
      }
      listLength + treeDepth
    }
  }

  def empty[A]: TreeList[A] = Empty

  val Empty: TreeList[Nothing] = Trees[Nothing](Nil)

  object NonEmpty {
    def apply[A](head: A, tail: TreeList[A]): TreeList[A] = head :: tail
    def unapply[A](fa: TreeList[A]): Option[(A, TreeList[A])] =
      fa.uncons
  }

  def fromList[A](list: List[A]): TreeList[A] =
    fromListReverse(list.reverse)

  def fromListReverse[A](list: List[A]): TreeList[A] = {
    @tailrec
    def loop(rev: List[A], acc: TreeList[A]): TreeList[A] =
      rev match {
        case Nil       => acc
        case h :: tail => loop(tail, acc.prepend(h))
      }

    loop(list, empty)
  }

  @inline private def toListOfTrees[A](ts: TreeList[A]): List[Tree[Nat, A]] =
    ts match {
      case Trees(tl) => tl
    }

  implicit def catsCollectionTreeListOrder[A: Order]: Order[TreeList[A]] =
    new Order[TreeList[A]] {
      val ordA: Order[A] = Order[A]
      @tailrec
      def compare(l: TreeList[A], r: TreeList[A]): Int = {
        (l.uncons, r.uncons) match {
          case (None, None)    => 0
          case (Some(_), None) => 1
          case (None, Some(_)) => -1
          case (Some(l0, l1), Some(r0, r1)) =>
            val c0 = ordA.compare(l0, r0)
            if (c0 == 0) compare(l1, r1)
            else c0
        }
      }
    }

  // This is here because it needs to see Tree and Nat
  private[collections] def eqTree[A: Eq]: Eq[TreeList[A]] =
    Eq[List[Tree[Nat, A]]].contramap(toListOfTrees(_))

  implicit def catsCollectionTreeListMoniod[A]: Monoid[TreeList[A]] =
    new Monoid[TreeList[A]] {
      def empty: TreeList[A] = Empty
      def combine(l: TreeList[A], r: TreeList[A]) = l ++ r
    }

  implicit def catsCollectionTreeListShow[A: Show]: Show[TreeList[A]] =
    Show.show[TreeList[A]] { ts =>
      val sa = Show[A]
      ts.toIterator.map(sa.show(_)).mkString("TreeList(", ", ", ")")
    }

  implicit val catsCollectionTreeListInstances: Traverse[TreeList]
    with Alternative[TreeList]
    with Monad[TreeList]
    with CoflatMap[TreeList]
    with FunctorFilter[TreeList] =
    new Traverse[TreeList]
      with Alternative[TreeList]
      with Monad[TreeList]
      with CoflatMap[TreeList]
      with FunctorFilter[TreeList] {
      def coflatMap[A, B](fa: TreeList[A])(fn: TreeList[A] => B): TreeList[B] = {
        @tailrec
        def loop(fa: TreeList[A], revList: List[B]): TreeList[B] =
          fa match {
            case NonEmpty(_, tail) =>
              loop(tail, fn(fa) :: revList)
            case Empty => fromListReverse(revList)
          }
        loop(fa, Nil)
      }

      def combineK[A](l: TreeList[A], r: TreeList[A]): TreeList[A] =
        l ++ r

      def empty[A]: TreeList[A] = Empty

      override def exists[A](fa: TreeList[A])(fn: A => Boolean): Boolean =
        fa.toIterator.exists(fn)

      override def flatMap[A, B](fa: TreeList[A])(fn: A => TreeList[B]): TreeList[B] =
        fa.flatMap(fn)

      def foldLeft[A, B](fa: TreeList[A], init: B)(fn: (B, A) => B): B =
        fa.foldLeft(init)(fn)

      override def foldMap[A, B: Monoid](fa: TreeList[A])(fn: A => B): B =
        fa.foldMap(fn)

      def foldRight[A, B](fa: TreeList[A], fin: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: List[Tree[Nat, A]]): Eval[B] =
          as match {
            case Nil => fin
            case Root(a) :: tail =>
              fn(a, Eval.defer(loop(tail)))
            case Balanced(a, l, r) :: tail =>
              fn(a, Eval.defer(loop(l :: r :: tail)))
          }
        loop(toListOfTrees(fa))
      }

      override def forall[A](fa: TreeList[A])(fn: A => Boolean): Boolean = {
        fa.toIterator.forall(fn)
      }

      def functor: Functor[TreeList] = this

      def mapFilter[A, B](ta: TreeList[A])(fn: A => Option[B]): TreeList[B] = {
        val as = ta.toIterator
        var resList = List.empty[B]
        while (as.hasNext) {
          fn(as.next()) match {
            case Some(b) => resList = b :: resList
            case None    => ()
          }
        }
        TreeList.fromListReverse(resList)
      }

      override def filter[A](ta: TreeList[A])(fn: A => Boolean): TreeList[A] =
        ta.filter(fn)

      override def get[A](fa: TreeList[A])(idx: Long): Option[A] =
        fa.get(idx)

      override def isEmpty[A](fa: TreeList[A]): Boolean = fa.isEmpty

      override def map[A, B](fa: TreeList[A])(fn: A => B): TreeList[B] =
        fa.map(fn)

      override def nonEmpty[A](fa: TreeList[A]): Boolean = fa.nonEmpty

      def pure[A](a: A): TreeList[A] =
        Trees(Root(a) :: Nil)

      override def reduceLeftToOption[A, B](fa: TreeList[A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.uncons match {
          case None => None
          case Some(a, tail) =>
            Some {
              if (tail.isEmpty) f(a)
              else tail.foldLeft(f(a))(g)
            }
        }

      override def reduceRightToOption[A, B](fa: TreeList[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        fa.uncons match {
          case None => Eval.now(None)
          case Some(a, tail) =>
            if (tail.isEmpty) Eval.now(Some(f(a)))
            else foldRight(tail, Eval.now(f(a)))(g).map(Some(_))
        }

      override def toList[A](fa: TreeList[A]): List[A] =
        fa.toList

      override def sequence_[G[_], A](fa: TreeList[G[A]])(implicit G: Applicative[G]): G[Unit] = {
        def loop(treeList: List[Tree[Nat, G[A]]]): G[Unit] =
          treeList match {
            case Nil => G.unit
            case Root(a) :: tail =>
              a *> loop(tail)
            case Balanced(a, l, r) :: tail =>
              a *> loop(l :: r :: tail)
          }

        loop(toListOfTrees(fa))
      }

      def tailRecM[A, B](a: A)(fn: A => TreeList[Either[A, B]]): TreeList[B] = {
        @tailrec
        def loop(stack: List[TreeList[Either[A, B]]], acc: List[B]): List[B] =
          stack match {
            case head :: tail =>
              head match {
                case NonEmpty(either, rest) =>
                  either match {
                    case Right(b) =>
                      loop(rest :: tail, b :: acc)
                    case Left(a) =>
                      loop(fn(a) :: rest :: tail, acc)
                  }
                case Empty =>
                  loop(tail, acc)
              }
            case Nil => acc
          }

        val res = loop(fn(a) :: Nil, Nil)
        fromListReverse(res)
      }

      override def traverse_[G[_], A, B](fa: TreeList[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = {
        def loop(treeList: List[Tree[Nat, A]]): G[Unit] =
          treeList match {
            case Nil => G.unit
            case Root(a) :: tail =>
              f(a) *> loop(tail)
            case Balanced(a, l, r) :: tail =>
              f(a) *> loop(l :: r :: tail)
          }

        loop(toListOfTrees(fa))
      }

      def traverse[G[_], A, B](fa: TreeList[A])(f: A => G[B])(implicit G: Applicative[G]): G[TreeList[B]] = {
        def loop(treeList: List[Tree[Nat, A]]): G[List[Tree[Nat, B]]] =
          treeList match {
            case Nil => G.pure(Nil)
            case head :: tail =>
              (traverseTree(head, f), loop(tail)).mapN(_ :: _)
          }

        loop(toListOfTrees(fa)).map(Trees(_))
      }
    }
}

abstract private[collections] class TreeListInstances0 extends TreeListInstances1 {
  implicit def catsCollectionTreeListPartialOrder[A: PartialOrder]: PartialOrder[TreeList[A]] =
    new PartialOrder[TreeList[A]] {
      val ordA: PartialOrder[A] = PartialOrder[A]
      def partialCompare(l: TreeList[A], r: TreeList[A]): Double = {
        @tailrec
        def loop(l: TreeList[A], r: TreeList[A]): Double =
          (l.uncons, r.uncons) match {
            case (None, None)    => 0.0
            case (Some(_), None) => 1.0
            case (None, Some(_)) => -1.0
            case (Some(l0, l1), Some(r0, r1)) =>
              val c0 = ordA.partialCompare(l0, r0)
              if (c0 == 0.0) loop(l1, r1)
              else c0
          }
        loop(l, r)
      }
    }
}

abstract private[collections] class TreeListInstances1 {
  implicit def catsCollectionTreeListEq[A: Eq]: Eq[TreeList[A]] =
    TreeList.eqTree
}
