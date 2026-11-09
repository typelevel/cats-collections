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
  Apply,
  Eq,
  Eval,
  Monad,
  NonEmptyAlternative,
  NonEmptyTraverse,
  StackSafeMonad,
  Traverse
}

import cats.data.Chain
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3
import scala.collection.immutable.LinearSeq
import org.typelevel.scalaccompat.annotation._
import scala.collection.TraversableOnce
import java.util.concurrent.atomic.AtomicBoolean

sealed trait BList[+A] {
  def uncons: Option[(A, BList[A])]
  def prepend[B >: A](a: B): BList.NonEmpty[B]
  def headOption: Option[A]
  def tailOption: Option[BList[A]]
  def headUnsafe: A
  def tailUnsafe: BList[A]
  def get(idx: Long): Option[A]
  def getUnsafe(idx: Long): A
  def reverse: BList[A]
  def splitAt(idx: Int): (BList[A], BList[A])
  def lastOption: Option[A]
  def size: Long
  def map[B](fn: A => B): BList[B]
  def filter(fn: A => Boolean): BList[A]
  def filterNot(fn: A => Boolean): BList[A]
  def collect[B](pf: PartialFunction[A, B]): BList[B]
  def foldLeft[B](init: B)(fn: (B, A) => B): B
  def foldRight[B](init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  def drop(n: Int): BList[A]
  def dropWhile(p: A => Boolean): BList[A]
  def take(n: Int): BList[A]
  def takeWhile(p: A => Boolean): BList[A]
  def concat[B >: A](l2: BList[B]): BList[B]
  def toList: List[A]
  def toListReverse: List[A]
  def asSeq: LinearSeq[A]
  def isEmpty: Boolean
  def flatMap[B](fn: A => BList[B]): BList[B]
  def foreach[B](f: A => B): Unit
  def iterator: Iterator[A]

  final def ::[B >: A](a: B): BList.NonEmpty[B] = prepend(a)
  final def ++[B >: A](l2: BList[B]): BList[B] = concat(l2)

  // for development and testing
  private[collections] def toStringInBlocks: String

  override def toString: String = {
    val strb = new java.lang.StringBuilder
    strb.append("BList(")
    @tailrec
    def loop(first: Boolean, l: BList[A]): Unit =
      l.uncons match {
        case None         => ()
        case Some((h, t)) =>
          if (!first) strb.append(", "): Unit
          strb.append(h.toString)
          loop(false, t)
      }

    loop(true, this)
    strb.append(")")
    strb.toString
  }

  def equals(other: Any): Boolean
  def hashCode(): Int
}

object BList {
  final private[collections] val BlockSize = 36

  case object Empty extends BList[Nothing] {
    def uncons: None.type = None
    def prepend[B >: Nothing](a: B): BList.NonEmpty[B] = {
      val ary = new Array[Any](BlockSize)
      val offset = BlockSize - 1
      ary(offset) = a
      Impl(offset, ary, Empty)
    }
    def headOption: None.type = None
    def tailOption: None.type = None
    def headUnsafe: Nothing = throw new NoSuchElementException // follows scala List's head
    def tailUnsafe: BList[Nothing] = throw new NoSuchElementException // follows scala List's tail
    def get(idx: Long): None.type = None
    def getUnsafe(idx: Long): Nothing = throw new IndexOutOfBoundsException
    def reverse: Empty.type = Empty
    def splitAt(idx: Int): (Empty.type, Empty.type) = (Empty, Empty)
    def lastOption: None.type = None
    def size: Long = 0
    def map[B](fn: Nothing => B): Empty.type = this
    def filter(fn: Nothing => Boolean): Empty.type = this
    def filterNot(fn: Nothing => Boolean): Empty.type = this
    def collect[B](pf: PartialFunction[Nothing, B]): Empty.type = this
    def foldLeft[B](acc: B)(fn: (B, Nothing) => B): B = acc
    def foldRight[B](init: Eval[B])(f: (Nothing, Eval[B]) => Eval[B]): Eval[B] = init
    def drop(n: Int): Empty.type = this
    def dropWhile(p: Nothing => Boolean): Empty.type = this
    def take(n: Int): Empty.type = this
    def takeWhile(p: Nothing => Boolean): Empty.type = this
    def concat[B](l2: BList[B]): BList[B] = l2
    override def toList: Nil.type = Nil
    override def toListReverse: Nil.type = Nil
    def asSeq: LinearSeq[Nothing] = LinearSeq.empty
    def isEmpty: Boolean = true
    def flatMap[B](fn: Nothing => BList[B]): Empty.type = this
    def foreach[B](f: Nothing => B): Unit = ()

    def iterator: Iterator[Nothing] = Iterator.empty
    private[collections] def toStringInBlocks: String = "Empty"

    override def equals(other: Any): Boolean = other.isInstanceOf[Empty.type]

    override def hashCode(): Int = Nil.##

  }
  sealed trait NonEmpty[+A] extends BList[A] {
    def head: A
    def tail: BList[A]
    def reverse: BList.NonEmpty[A]
    def map[B](fn: A => B): BList.NonEmpty[B]
    def concat[B >: A](l2: BList[B]): BList.NonEmpty[B]
    def uncons: Some[(A, BList[A])]
    def headOption: Some[A]
    def tailOption: Some[BList[A]]
    def flatMapNonEmpty[B](fn: A => NonEmpty[B]): NonEmpty[B]
    final def ++[B >: A](l2: NonEmpty[B]): NonEmpty[B] = concat(l2)
  }

  object NonEmpty {
    def apply[A](h: A, t: BList[A]): NonEmpty[A] =
      t.prepend(h)
    def unapply[A](l: NonEmpty[A]): Some[(A, BList[A])] =
      l.uncons

    implicit def eqNonEmptyBList[B]: Eq[NonEmpty[B]] =
      new Eq[NonEmpty[B]] {
        def eqv(xs: NonEmpty[B], ys: NonEmpty[B]): Boolean = xs.equals(ys)
      }

    @nowarn213(
      "msg=Calls to parameterless method compose will be easy to mistake for calls to overloads which have a single implicit parameter list"
    )
    implicit val catsCollectionNonEmptyBListInstances
      : NonEmptyTraverse[NonEmpty] & NonEmptyAlternative[NonEmpty] & Apply[NonEmpty] =
      new NonEmptyTraverse[NonEmpty] with NonEmptyAlternative[NonEmpty] with Apply[NonEmpty] {
        override def map[A, B](fa: NonEmpty[A])(f: (A) => B): NonEmpty[B] = fa.map(f)
        override def ap[A, B](ff: NonEmpty[(A) => B])(xs: NonEmpty[A]): NonEmpty[B] =
          ff.flatMapNonEmpty(f => xs.map(a => f(a)))
        override def pure[A](x: A): NonEmpty[A] = Empty.prepend(x)
        override def combineK[A](x: NonEmpty[A], y: NonEmpty[A]): NonEmpty[A] = x.concat(y)
        override def foldLeft[A, B](xs: NonEmpty[A], init: B)(f: (B, A) => B): B =
          xs.foldLeft(init)(f)
        override def foldRight[A, B](xs: NonEmpty[A], init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          xs.foldRight(init)(f)
        override def nonEmptyTraverse[G[_], A, B](
          fa: NonEmpty[A]
        )(f: (A) => G[B])(implicit arg0: Apply[G]): G[NonEmpty[B]] = {
          def traverseArray(arr: Array[A]): G[Array[Any]] = {
            arr.tail.foldLeft(arg0.map(f(arr.head))(b => Array[Any](b))) { (arrEffect, head) =>
              arg0.map2(arrEffect, f(head)) { (arr, b) =>
                arr :+ b
              }
            }
          }
          def loop(l: NonEmpty[A]): Eval[G[NonEmpty[B]]] = l match {
            case impl: AbstractImpl[A] =>
              impl.tailBList match {
                case Empty =>
                  Eval.now(arg0.map(traverseArray(impl.block.slice(impl.offset, BlockSize)).asInstanceOf[G[Array[B]]]) {
                    arrayB =>
                      Impl(impl.offset, new Array[Any](impl.offset) ++ arrayB, BList.empty)
                  })
                case nonemptytail: AbstractImpl[A] =>
                  arg0 match {
                    case x: StackSafeMonad[G] => // optimization described in issue #4480
                      Eval.now(
                        x.map(
                          fa.iterator
                            .foldLeft(x.pure(Chain.empty[B])) { case (accG, a) =>
                              x.map2(accG, f(a)) { case (acc, b) =>
                                acc :+ b
                              }
                            }
                        )(c => from(c.iterator))
                          .asInstanceOf[G[NonEmpty[B]]]
                      )

                    case _ =>
                      val traversedArray =
                        traverseArray(impl.block.slice(impl.offset, BlockSize)).asInstanceOf[G[Array[B]]]
                      Eval.defer(loop(nonemptytail)).map { tailB =>
                        arg0.map2(traversedArray, tailB) { (arrayB, tailB) =>
                          Impl(impl.offset, new Array[Any](impl.offset) ++ arrayB, tailB)
                        }
                      }
                  }
              }
          }
          loop(fa).value
        }
        override def nonEmptyTraverseVoid[G[_], A, B](
          fa: NonEmpty[A]
        )(f: (A) => G[B])(implicit arg0: Apply[G]): G[Unit] = {
          def traverseBlockVoid(offset: Int, block: Array[A]): G[Unit] = {
            var i = offset + 1
            var acc = arg0.map(f(block(offset))) { _ => () }

            while (i < BlockSize) {
              acc = arg0.map2(acc, f(block(i)))((_, _) => ())
              i += 1
            }
            acc
          }
          def loop(l: NonEmpty[A]): Eval[G[Unit]] = l match {
            case impl: AbstractImpl[A] =>
              impl.tailBList match {
                case Empty                         => Eval.now(traverseBlockVoid(impl.offset, impl.block))
                case nonemptytail: AbstractImpl[A] =>
                  val traversedBlock = traverseBlockVoid(impl.offset, impl.block)
                  Eval.defer(loop(nonemptytail)).map {
                    arg0.map2(traversedBlock, _)((_, _) => ())
                  }
              }
          }
          loop(fa).value
        }
        override def nonEmptyTraverse_[G[_], A, B](fa: NonEmpty[A])(f: (A) => G[B])(implicit arg0: Apply[G]): G[Unit] =
          nonEmptyTraverseVoid(fa)(f)(arg0)

        override def reduceLeftTo[A, B](fa: NonEmpty[A])(f: (A) => B)(g: (B, A) => B): B =
          fa.tail.foldLeft(f(fa.head))(g)

        override def reduceRightTo[A, B](fa: NonEmpty[A])(f: (A) => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
          fa.tail.foldRight(Eval.now(f(fa.head)))(g)
      }

  }

  private object Impl {
    def apply[A](offset: Int, block: Array[A], tailBList: BList[A]): Impl[A] =
      new Impl(offset, block, tailBList)
    def apply[A](offset: Int, block: Array[Any], tailBList: BList[A]): Impl[A] =
      new Impl(offset, block.asInstanceOf[Array[A]], tailBList)
    def unapply[A](l: Impl[A]): Some[(Int, Array[A], BList[A])] =
      Some((l.offset, l.block, l.tailBList))
  }
  private object MutableImpl {
    def apply[A](offset: Int, block: Array[A], tailBList: BList[A]): MutableImpl[A] =
      new MutableImpl(offset, block, tailBList)
    def apply[A](offset: Int, block: Array[Any], tailBList: BList[A]): MutableImpl[A] =
      new MutableImpl(offset, block.asInstanceOf[Array[A]], tailBList)
    def unapply[A](l: MutableImpl[A]): Some[(Int, Array[A], BList[A])] =
      Some((l.offset, l.block, l.tailBList))
  }

  private class MutableImpl[+A](offset: Int,
                                block: Array[A @uncheckedVariance],
                                var tailBList: BList[A] @uncheckedVariance
  ) extends AbstractImpl[A](offset, block)
  private class Impl[+A](offset: Int, block: Array[A @uncheckedVariance], val tailBList: BList[A])
      extends AbstractImpl[A](offset, block)

  sealed abstract private class AbstractImpl[+A](val offset: Int, val block: Array[A @uncheckedVariance])
      extends AtomicBoolean(false)
      with NonEmpty[A] { shared =>
    def tailBList: BList[A]

    override def equals(other: Any): Boolean =
      other match {
        case o: AbstractImpl[_] =>
          this.iterator.sameElements(o.iterator.asInstanceOf[Iterator[A]])
        case _ => false
      }

    override def hashCode: Int =
      MurmurHash3.orderedHash(this.iterator)

    def uncons: Some[(A, BList[A])] = {
      Some((block(offset), this.tail))
    }

    def prepend[B >: A](a: B): BList.NonEmpty[B] = {
      if (offset > 0) {
        val nextOffset = offset - 1
        if (shared.compareAndSet(false, true)) { // not shared
          block(nextOffset) = a.asInstanceOf[A]
          Impl(nextOffset, block, tailBList)
        } else { // shared, forking detected
          val ary = new Array[Any](BlockSize)
          System.arraycopy(block, offset, ary, offset, BlockSize - offset)
          ary(nextOffset) = a
          Impl(nextOffset, ary, tailBList)
        }
      } else {
        val ary = new Array[Any](BlockSize)
        val offset = BlockSize - 1
        ary(offset) = a
        Impl(offset, ary, this)
      }
    }

    def head: A = {
      block(offset)
    }
    def tail: BList[A] = {
      if (offset < BlockSize - 1) {
        val ary = new Array[Any](BlockSize)
        System.arraycopy(block, offset + 1, ary, offset + 1, BlockSize - (offset + 1))
        Impl(offset + 1, ary, tailBList)
      } else {
        tailBList
      }
    }
    def headOption: Some[A] = {
      Some(block(offset))
    }
    def tailOption: Some[BList[A]] = {
      Some(tail)
    }
    def headUnsafe: A = head
    def tailUnsafe: BList[A] = tail

    def get(idx: Long): Option[A] = {
      if (idx < 0) { None }
      else {
        @tailrec
        def go(idx: Long, l: BList[A]): Option[A] = {
          l match {
            case Empty                            => None
            case impl: AbstractImpl[A] @unchecked =>
              if (idx < BlockSize - impl.offset) {
                Some(impl.block(impl.offset + idx.toInt))
              } else {
                go(idx - (BlockSize - impl.offset), impl.tailBList)
              }
          }
        }
        go(idx, this)
      }
    }
    def getUnsafe(idx: Long): A = {
      if (idx < 0)
        throw new IndexOutOfBoundsException

      @tailrec
      def go(idx: Long, l: BList[A]): A = {
        l match {
          case Empty                            => throw new IndexOutOfBoundsException
          case impl: AbstractImpl[A] @unchecked =>
            if (idx < BlockSize - impl.offset) {
              impl.block(impl.offset + idx.toInt)
            } else {
              go(idx - (BlockSize - impl.offset), impl.tailBList)
            }
        }
      }
      go(idx, this)
    }
    def reverse: BList.NonEmpty[A] = {
      @tailrec
      def go(l: BList[A], acc: BList[A]): BList.NonEmpty[A] = l match {
        case Empty                            => acc.asInstanceOf[NonEmpty[A]]
        case impl: AbstractImpl[A] @unchecked => // strategy: prepend blocks with reversed arrays
          val ary = impl.block.clone() // zero-ed out memory is consistent under reversal
          var i: Int = impl.offset
          var end: Int = BlockSize - 1
          while (i < BlockSize) {
            ary(i) = impl.block(end)
            i += 1
            end -= 1
          }
          go(impl.tailBList, Impl(impl.offset, ary, acc))
      }
      go(this, Empty)
    }
    def splitAt(idx: Int): (BList[A], BList[A]) = {
      def buildLists(idx: Int, l: BList[A]): Eval[(BList[A], BList[A])] = {
        l match {
          case Empty                            => Eval.now((Empty, Empty))
          case impl: AbstractImpl[A] @unchecked =>
            if (idx < BlockSize - impl.offset) { // we found the block we want to split at
              if (idx <= 0) { // split is on block boundary
                Eval.now((Empty, impl))
              } else {
                val ary1 = new Array[Any](BlockSize)
                val ary2 = new Array[Any](BlockSize)
                System.arraycopy(impl.block, impl.offset, ary1, BlockSize - idx, idx)
                System.arraycopy(impl.block,
                                 impl.offset + idx,
                                 ary2,
                                 impl.offset + idx,
                                 (BlockSize - impl.offset) - idx
                )
                val lastBlockl1 = Impl(BlockSize - idx.toInt, ary1, Empty)
                val l2 = Impl(impl.offset + idx.toInt, ary2, impl.tailBList)
                Eval.now((lastBlockl1, l2))
              }
            } else { // split isn't in current block
              Eval.defer(buildLists(idx - (BlockSize - impl.offset), impl.tailBList)).map { case (l1_tail, l2) =>
                (Impl(impl.offset, impl.block, l1_tail), l2)
              }
            }
        }
      }
      buildLists(idx, this).value
    }
    def lastOption: Some[A] = {
      @tailrec
      def go(self: AbstractImpl[A]): Some[A] = {
        self.tailBList match {
          case Empty                            => Some(self.block(BlockSize - 1))
          case next: AbstractImpl[A] @unchecked => go(next)
        }
      }
      go(this)
    }
    def size: Long = {
      @tailrec
      def loop(l: BList[A], acc: Long): Long = {
        l match {
          case Empty                            => acc
          case impl: AbstractImpl[A] @unchecked => loop(impl.tailBList, acc + (BlockSize - impl.offset))
        }
      }
      loop(this, 0L)
    }
    def map[B](fn: A => B): BList.NonEmpty[B] = {
      val builder = BList.newBuilder[B]
      @tailrec
      def loop(l: BList[A]): BList[B] = {
        l match {
          case Empty                 => builder.result()
          case impl: AbstractImpl[A] =>
            for (i <- impl.offset until BlockSize) {
              builder += fn(impl.block(i))
            }
            loop(impl.tailBList)
        }
      }
      loop(this).asInstanceOf[NonEmpty[B]]
    }
    def filter(p: A => Boolean): BList[A] = {
      def go(l: BList[A], prev: Array[Any], prevOffset: Int): Eval[BList[A]] = l match {
        case Empty =>
          if (prevOffset == BlockSize) {
            Eval.now(Empty) // prev is invalid
          } else {
            Eval.now(Impl(prevOffset, prev, Empty))
          }
        case impl: AbstractImpl[A] =>

          var i = BlockSize - 1
          var offset_in_newblock = BlockSize
          val newblock = new Array[Any](BlockSize)
          // iterate backwards
          while (i >= impl.offset) {
            if (p(impl.block(i))) {
              offset_in_newblock -= 1
              newblock(offset_in_newblock) = impl.block(i)
            }
            i -= 1
          }

          if (offset_in_newblock == BlockSize) { // all elements were filtered out, node is skipped
            Eval.defer(go(impl.tailBList, prev, prevOffset))
          } else if ((BlockSize - offset_in_newblock) + (BlockSize - prevOffset) <= BlockSize) {
            // combine prev with new and rec call
            System.arraycopy(prev,
                             prevOffset,
                             newblock,
                             offset_in_newblock - (BlockSize - prevOffset),
                             BlockSize - prevOffset
            )
            Eval.defer(go(impl.tailBList, newblock, offset_in_newblock - (BlockSize - prevOffset)))
          } else {
            // add previous to acc and make rec call
            Eval.defer(go(impl.tailBList, newblock, offset_in_newblock)).map(Impl(prevOffset, prev, _))
          }
      }

      go(this, new Array[Any](BlockSize), BlockSize).value
    }
    def filterNot(p: A => Boolean): BList[A] = {
      filter(x => !p(x))
    }
    // adapted from scala List's implementation of collect https://github.com/scala/scala/blob/2.13.x/src/library/scala/collection/immutable/List.scala#L250
    private val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }
    def collect[B](pf: PartialFunction[A, B]): BList[B] = {
      def go(l: BList[A]): Eval[BList[B]] = l match {
        case Empty                 => Eval.now(Empty)
        case impl: AbstractImpl[A] =>
          var i = BlockSize - 1
          var offset_in_newblock = BlockSize
          val newblock = new Array[Any](BlockSize)
          var x: Any = null
          // iterate backwards, building new array
          while (i >= impl.offset) {
            x = pf.applyOrElse(impl.block(i), partialNotApplied)
            if (x.asInstanceOf[AnyRef] ne partialNotApplied) {
              offset_in_newblock -= 1
              newblock(offset_in_newblock) = x.asInstanceOf[B]
            }
            i -= 1
          }

          if (offset_in_newblock == BlockSize) { // new block is empty so we skip it
            Eval.defer(go(impl.tailBList))
          } else {
            Eval.defer(go(impl.tailBList)).map(Impl(offset_in_newblock, newblock, _))
          }
      }
      go(this).value
    }
    def foldLeft[B](acc: B)(fn: (B, A) => B): B = {
      @tailrec
      def loop(acc: B, l: BList[A]): B =
        l match {
          case Empty                            => acc
          case impl: AbstractImpl[A] @unchecked =>
            var newacc = acc
            var i = impl.offset
            while (i < BlockSize) {
              newacc = fn(newacc, impl.block(i))
              i += 1
            }
            loop(newacc, impl.tailBList)
        }
      loop(acc, this)
    }
    def foldRight[B](init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def go(init: Eval[B], l: BList[A]): Eval[B] =
        l match {
          case Empty                            => init
          case impl: AbstractImpl[A] @unchecked =>
            def loop(idx: Int): Eval[B] =
              if (idx >= BlockSize) Eval.defer(go(init, impl.tailBList))
              else Eval.defer(f(impl.block(idx), loop(idx + 1)))
            loop(impl.offset)
        }
      go(init, this)
    }
    def drop(n: Int): BList[A] = {
      @tailrec
      def go(n: Int, l: BList[A]): BList[A] = {
        if (n <= 0) {
          l
        } else {
          l match {
            case Empty =>
              Empty
            case impl: AbstractImpl[A] @unchecked =>
              if (n >= BlockSize - impl.offset) {
                go(n - (BlockSize - impl.offset), impl.tailBList)
              } else {
                val ary = new Array[Any](BlockSize)
                val newOffset: Int = impl.offset + n
                System.arraycopy(impl.block, newOffset, ary, newOffset, BlockSize - newOffset)
                Impl(newOffset, ary, impl.tailBList)
              }

          }
        }
      }
      go(n, this)
    }
    def dropWhile(p: A => Boolean): BList[A] = {
      @tailrec
      def go(l: BList[A]): BList[A] = {
        l match {
          case Empty =>
            Empty
          case impl: AbstractImpl[A] @unchecked =>
            var i = impl.offset
            var cont = true

            // search for first falsified index
            while (i < BlockSize && cont) {
              if (!p(impl.block(i))) {
                // break loop
                cont = false
              } else i += 1
            }

            if (i == BlockSize) { // all elements satifsy predicate so continue looking at next block
              go(impl.tailBList)
            } else if (i > impl.offset) { // some elements sat pred, make new array and return the list from there
              val ary = new Array[Any](BlockSize)
              val newOffset: Int = i // first index where predicate was falsified
              System.arraycopy(impl.block, newOffset, ary, newOffset, BlockSize - newOffset)
              Impl(newOffset, ary, impl.tailBList)
            } else { // i is offset and none of the elements satisfy predicate, so return the list starting here
              impl
            }
        }
      }
      go(this)
    }
    def take(m: Int): BList[A] = {
      def go(n: Int, l: BList[A]): Eval[BList[A]] =
        l match {
          case Empty                 => Eval.now(Empty)
          case impl: AbstractImpl[A] =>
            if (n <= 0) {
              Eval.now(Empty)
            } else if (n >= BlockSize - impl.offset) {
              Eval.defer(go(n - (BlockSize - impl.offset), impl.tailBList)).map(Impl(impl.offset, impl.block, _))
            } else {
              val ary = new Array[Any](BlockSize)
              System.arraycopy(impl.block, impl.offset, ary, BlockSize - n, n)
              Eval.now(Impl(BlockSize - n, ary, Empty))
            }
        }

      go(m, this).value
    }

    def takeWhile(p: A => Boolean): BList[A] = {
      def go(l: BList[A]): Eval[BList[A]] = l match {
        case Empty                 => Eval.now(Empty)
        case impl: AbstractImpl[A] =>
          var i = impl.offset
          var cont = true

          // search for first falsified index
          while (i < BlockSize && cont) {
            if (!p(impl.block(i))) {
              cont = false // break loop
            } else i += 1
          }

          if (i >= BlockSize) { // all elmts in block satisfied, keep taking
            Eval.defer(go(impl.tailBList)).map(Impl(impl.offset, impl.block, _))
          } else if (i < impl.offset) { // no elements in block satisfied
            Eval.now(Empty)
          } else { // take some prefix of the block
            val ary = new Array[Any](BlockSize)
            System.arraycopy(impl.block, impl.offset, ary, BlockSize - (i - impl.offset), i - impl.offset)
            Eval.now(Impl(BlockSize - (i - impl.offset), ary, Empty))
          }
      }
      go(this).value
    }
    def concat[B >: A](l2: BList[B]): BList.NonEmpty[B] = {
      def go(self: AbstractImpl[A]): Eval[BList.NonEmpty[B]] = {
        self.tailBList match {
          case Empty =>
            Eval.now(Impl(self.offset, self.block.asInstanceOf[Array[Any]], l2))
          case next: AbstractImpl[A] @unchecked =>
            Eval.defer(go(next)).map(Impl(self.offset, self.block.asInstanceOf[Array[Any]], _))
        }
      }

      l2 match {
        case Empty              => this
        case _: AbstractImpl[_] => go(this).value
      }
    }

    override def toList: List[A] = {
      val builder = List.newBuilder[A]
      @tailrec
      def loop(l: BList[A]): List[A] =
        l match {
          case Empty                            => builder.result()
          case impl: AbstractImpl[A] @unchecked =>
            // append valid things in the block to acc
            for (i <- impl.offset until BlockSize) {
              builder += impl.block(i)
            }
            loop(impl.tailBList)
        }
      loop(this)
    }

    override def toListReverse: List[A] = {
      @tailrec
      def loop(xs: BList[A], acc: List[A]): List[A] =
        xs match {
          case Empty =>
            acc
          case impl: AbstractImpl[A] @unchecked =>
            var acc2 = acc
            var i = impl.offset
            while (i < BlockSize) {
              acc2 = impl.block(i) :: acc2
              i += 1
            }
            loop(impl.tailBList, acc2)
        }
      loop(this, Nil)
    }

    def asSeq: LinearSeq[A] = new BListSeq(this)

    def isEmpty: Boolean = false

    def flatMap[B](fn: A => BList[B]): BList[B] = {
      @tailrec
      def loop(rev: List[A], acc: BList[B]): BList[B] =
        rev match {
          case Nil       => acc
          case h :: tail =>
            loop(tail, fn(h) ++ acc)
        }
      loop(this.toListReverse, BList.empty)
    }
    def flatMapNonEmpty[B](fn: A => NonEmpty[B]): NonEmpty[B] = {
      @tailrec
      def loop(rev: List[A], acc: NonEmpty[B]): NonEmpty[B] =
        rev match {
          case Nil       => acc
          case h :: tail =>
            loop(tail, fn(h) ++ acc)
        }
      val revList = this.toListReverse
      loop(revList.tail, fn(revList.head))
    }
    def foreach[B](f: A => B): Unit = {
      @tailrec
      def loop(l: BList[A]): Unit = {
        l match {
          case Empty                 => ()
          case impl: AbstractImpl[A] =>
            for (i <- impl.offset until BlockSize) {
              f(impl.block(i))
            }
            loop(impl.tailBList)
        }
      }
      loop(this)
    }

    def iterator: Iterator[A] = new BListIterator(this)

    private[collections] def toStringInBlocks: String = {
      val strb = new java.lang.StringBuilder
      strb.append("BList(")
      @tailrec
      def loop(first: Boolean, l: BList[A]): Unit = {
        if (!first) strb.append(", "): Unit
        l match {
          case Empty                            => strb.append("Empty"): Unit
          case impl: AbstractImpl[A] @unchecked =>
            strb.append("Block(")
            strb.append(impl.block(impl.offset).toString)
            for (i <- impl.offset + 1 until BlockSize) {
              strb.append(", ")
              strb.append(impl.block(i).toString)
            }
            strb.append(")")
            loop(false, impl.tailBList)
        }
      }
      loop(true, this)
      strb.append(")")
      strb.toString
    }
    final private class BListIterator(var curNode: AbstractImpl[A @uncheckedVariance]) extends Iterator[A] {
      var curOffset: Int = curNode.offset

      override def hasNext: Boolean = curOffset < BlockSize
      override def next(): A =
        if (curOffset >= BlockSize) {
          throw new NoSuchElementException
        } else {
          val next_elmt = curNode.block(curOffset)
          curOffset += 1
          if (curOffset >= BlockSize) {
            // try to advance to next block
            curNode.tailBList match {
              case impl: AbstractImpl[A] =>
                curOffset = impl.offset
                curNode = impl
              case Empty => // nothing, keep offset at blocksize
            }
          }
          next_elmt
        }
    }
    final private class BListSeq(private val xs: AbstractImpl[A @uncheckedVariance]) extends LinearSeq[A] {
      override def apply(i: Int): A = xs.getUnsafe(i.toLong)
      override def iterator: Iterator[A] = new BListIterator(xs)
      override def length: Int = xs.size.toInt
      override def head: A = xs.head
      override def tail: LinearSeq[A] = {
        xs.drop(1) match {
          case Empty                   => LinearSeq.empty
          case tlimpl: AbstractImpl[A] => new BListSeq(tlimpl)
        }
      }
      override def isEmpty: Boolean = false
    }
  }

  def fromList[A](l: List[A]): BList[A] = {
    val builder = BList.newBuilder[A]
    builder ++= l
    builder.result()
  }
  def fromListReverse[A](l: List[A]): BList[A] = {
    fromList(l.reverse)
  }

  def apply[A](elems: A*): BList[A] = {
    BList.from(elems)
  }

  // behaviour is undefined after result is called, same as for ListBuffer
  final class BListBuffer[A] {

    // im gonna build this array from left to right and if it is not full in result i will copy it over to the right
    // headoffset is the offset of the next element to be placed
    private var headIndex: Int = 0
    private var headArray: Array[Any] = new Array[Any](BlockSize)
    private var tail: BList[A] = BList.empty
    private var prev: MutableImpl[A] = null
    private var curIndex: Int = 0
    private var curArray: Array[Any] = new Array[Any](BlockSize)

    def addOne(elmt: A): this.type = {
      if (headIndex < BlockSize) { // elmt goes in head node
        headArray(headIndex) = elmt
        headIndex += 1
      } else { // elmt will not go in head node
        if (curIndex < BlockSize) { // current node is not full
          curArray(curIndex) = elmt
          curIndex += 1
        } else { // current node is full
          // finish off the full node
          val finishedNode = MutableImpl(0, curArray, null)
          if (prev != null) { // have previous node point to the newly completed one
            prev.tailBList = finishedNode
          }
          prev = finishedNode

          if (tail.isEmpty) { // fill in tail field with first completed node
            tail = prev
          }

          // start new node
          curArray = new Array[Any](BlockSize)
          curArray(0) = elmt
          curIndex = 1
        }
      }
      this
    }
    def clear(): Unit = {
      tail = BList.empty
      prev = null
      headIndex = 0
      headArray = new Array[Any](BlockSize)
      curIndex = 0
      curArray = new Array[Any](BlockSize)
    }

    def result(): BList[A] = {
      var finalheadary = headArray
      var finalheadoffset = 0

      if (headIndex == 0) { // result is empty
        Empty
      } else { // result is nonEmpty
        if (headIndex < BlockSize) { // head is incomplete
          // shift elmts over the be right-aligned in the array
          finalheadary = new Array[Any](BlockSize)
          System.arraycopy(headArray, 0, finalheadary, BlockSize - headIndex, headIndex)
          finalheadoffset = BlockSize - headIndex
        } else { // head is complete
          if (tail.isEmpty && curIndex == 0) { // the tail is empty
            // do nothing
          } else if (tail.isEmpty && curIndex > 0) { // there is one tail node and it is not empty
            val ary = new Array[Any](BlockSize)
            System.arraycopy(curArray,
                             0,
                             ary,
                             BlockSize - curIndex,
                             curIndex
            ) // curArray isnt null because curIndex > 0
            tail = MutableImpl(BlockSize - curIndex, ary.asInstanceOf[Array[A]], Empty)
          } else if (curIndex == BlockSize) { // final block is full
            prev.tailBList = MutableImpl(0, curArray, Empty) // safe because tail is not empty, so prev must not be null
          } else { // final block is not full
            val ary = new Array[Any](BlockSize)
            System.arraycopy(curArray, 0, ary, BlockSize - curIndex, curIndex)
            prev.tailBList = MutableImpl(BlockSize - curIndex,
                                         ary.asInstanceOf[Array[A]],
                                         Empty
            ) // safe because tail is not empty, so prev must not be null
          }
        }

        Impl(finalheadoffset, finalheadary, tail)
      }
    }

    @nowarn213("cat=deprecation")
    @nowarn3("cat=deprecation")
    def addAll(xs: TraversableOnce[A]): this.type = {
      val iter = xs.toIterator
      while (iter.hasNext) {
        if (headIndex < BlockSize) { // head has space
          headArray(headIndex) = iter.next()
          headIndex += 1
          while (iter.hasNext && headIndex < BlockSize) {
            headArray(headIndex) = iter.next()
            headIndex += 1
          }
        } else { // head is full
          if (curIndex < BlockSize) { // current node is not full
            while (iter.hasNext && curIndex < BlockSize) {
              curArray(curIndex) = iter.next()
              curIndex += 1
            }
          } else { // current node is full
            // finish off the full node
            val finishedNode = MutableImpl(0, curArray, null)
            if (prev != null) { // have previous node point to the newly completed one
              prev.tailBList = finishedNode
            }
            prev = finishedNode

            if (tail.isEmpty) { // fill in tail field with first completed node
              tail = prev
            }

            // start new node
            curIndex = 0
            curArray = new Array[Any](BlockSize)
            while (iter.hasNext && curIndex < BlockSize) {
              curArray(curIndex) = iter.next()
              curIndex += 1
            }
          }
        }
      }
      this
    }

    // aliases
    def +=(elmt: A): this.type = addOne(elmt)
    @nowarn213("cat=deprecation")
    @nowarn3("cat=deprecation")
    def ++=(xs: TraversableOnce[A]): this.type = addAll(xs)

  }

  @nowarn213("cat=deprecation")
  @nowarn3("cat=deprecation")
  def from[A](xs: TraversableOnce[A]): BList[A] = {
    val iter = xs.toIterator
    def go(): Eval[BList[A]] =
      if (iter.hasNext) {
        val ary = new Array[Any](BlockSize)
        var offset = 0
        while (offset < BlockSize && iter.hasNext) {
          ary(offset) = iter.next()
          offset += 1
        }
        if (offset < BlockSize) { // not full final block
          // need to copy to push to the end of array
          System.arraycopy(ary, 0, ary, BlockSize - offset, offset)
          Eval.defer(go()).map(Impl(BlockSize - offset, ary, _))
        } else { // block is full
          Eval.defer(go()).map(Impl(0, ary, _))
        }
      } else { // ran out of elmts
        Eval.now(Empty)
      }
    go().value
  }

  def empty[A]: BList[A] = Empty

  def newBuilder[A]: BListBuffer[A] =
    new BListBuffer[A]

  // Typeclasses

  implicit def eqBList[B]: Eq[BList[B]] =
    new Eq[BList[B]] {
      def eqv(xs: BList[B], ys: BList[B]): Boolean = xs.equals(ys)
    }

  @nowarn213(
    "msg=Calls to parameterless method compose will be easy to mistake for calls to overloads which have a single implicit parameter list"
  )
  implicit val catsCollectionBListInstances: Traverse[BList] & Alternative[BList] & Monad[BList] =
    new Traverse[BList] with Alternative[BList] with Monad[BList] {
      def foldLeft[A, B](xs: BList[A], init: B)(f: (B, A) => B): B =
        xs.foldLeft(init)(f)

      def foldRight[A, B](xs: BList[A], init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        xs.foldRight(init)(f)

      override def isEmpty[A](xs: BList[A]): Boolean = xs.isEmpty

      override def nonEmpty[A](xs: BList[A]): Boolean = !xs.isEmpty

      override def size[A](xs: BList[A]): Long = xs.size

      def traverse[G[_], A, B](fa: BList[A])(f: (A) => G[B])(implicit arg0: Applicative[G]): G[BList[B]] =
        fa match {
          case Empty                 => arg0.pure(BList.empty)
          case impl: AbstractImpl[A] =>
            NonEmpty.catsCollectionNonEmptyBListInstances.nonEmptyTraverse(impl)(f)(arg0).asInstanceOf[G[BList[B]]]
        }
      override def traverseVoid[G[_], A, B](fa: BList[A])(f: A => G[B])(implicit arg0: Applicative[G]): G[Unit] = {
        fa match {
          case Empty                 => arg0.unit
          case impl: AbstractImpl[A] =>
            NonEmpty.catsCollectionNonEmptyBListInstances.nonEmptyTraverseVoid(impl)(f)(arg0)
        }
      }
      override def traverse_[G[_], A, B](fa: BList[A])(f: A => G[B])(implicit arg0: Applicative[G]): G[Unit] =
        traverseVoid(fa)(f)(arg0)

      override def ap[A, B](ff: BList[(A) => B])(xs: BList[A]): BList[B] = ff.flatMap(f => xs.map(a => f(a)))

      def pure[A](x: A): BList[A] = Empty.prepend(x)

      def empty[A]: BList[A] = Empty

      def combineK[B](x: BList[B], y: BList[B]): BList[B] = x.concat(y)

      def flatMap[A, B](fa: BList[A])(f: (A) => BList[B]): BList[B] = fa.flatMap(f)

      override def map[B, C](a: BList[B])(f: B => C): BList[C] = a.map(f)

      // adapted from the implementation of tailRecM for ListInstances
      // https://github.com/typelevel/cats/blob/v0.7.0/core/src/main/scala/cats/instances/list.scala#L29
      def tailRecM[A, B](a: A)(f: (A) => BList[Either[A, B]]): BList[B] = {
        val buf = BList.newBuilder[B]
        @tailrec
        def go(lists: BList[BList[Either[A, B]]]): Unit = (lists: @unchecked) match {
          case Empty                                       => ()
          case BList.NonEmpty(BList.Empty, tail)           => go(tail.asInstanceOf[BList[BList[Either[A, B]]]])
          case BList.NonEmpty(impl: AbstractImpl[_], tail) => {
            var curoffset = impl.offset
            var prefix: BList[BList[Either[A, B]]] = BList(impl.tailBList.asInstanceOf[BList[Either[A, B]]])
            while (curoffset < BlockSize) {
              impl
                .block(curoffset)
                .asInstanceOf[Either[A, B]] match { // this is to prevent compiler warning about inexhuastive match
                case Right(b) =>
                  buf += b
                  curoffset += 1
                case Left(a_prime) =>
                  if (curoffset >= BlockSize - 1) {
                    prefix = BList(f(a_prime), impl.tailBList.asInstanceOf[BList[Either[A, B]]])
                  } else {
                    prefix = BList(f(a_prime),
                                   Impl(curoffset + 1, impl.block, impl.tailBList).asInstanceOf[BList[Either[A, B]]]
                    )
                  }
                  curoffset = BlockSize // to break out of loop
              }
            }
            go(prefix ++ tail.asInstanceOf[BList[BList[Either[A, B]]]])
          }
        }
        go(BList(f(a)))
        buf.result()
      }
    }
}
