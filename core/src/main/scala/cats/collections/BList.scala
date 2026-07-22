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

sealed abstract class BList[+A] {
  def uncons: Option[(A, BList[A])]
  def prepend[B >: A](a: B): BList.NonEmpty[B]
  def headOption: Option[A]
  def tailOption: Option[BList[A]]
  def get(idx: Long): Option[A]
  def getUnsafe(idx: Long): A
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

object BList extends compat.BListCompatCompanion {
  // final private[collections] val     <- these are removed for benchmarking differnt blocksizes against eachother
  final private[collections] val BlockSize = 16
  // var BlockSize = 4

  case object Empty extends BList[Nothing] {
    def uncons = None
    def prepend[B >: Nothing](a: B): BList.NonEmpty[B] = {
      val ary = new Array[Any](BlockSize)
      val offset = BlockSize - 1
      ary(offset) = a
      Impl(offset, ary, Empty)
    }
    def headOption: None.type = None
    def tailOption: None.type = None
    def get(idx: Long): None.type = None
    def getUnsafe(idx: Long): Nothing = throw new IndexOutOfBoundsException
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

    def iterator: Iterator[Nothing] = Iterator.empty
    private[collections] def toStringInBlocks: String = "Empty"

    override def equals(other: Any): Boolean = other.isInstanceOf[Empty.type]

    override def hashCode(): Int = Nil.##

  }
  sealed abstract class NonEmpty[+A] extends BList[A] {
    def head: A
    def tail: BList[A]
    def map[B](fn: A => B): BList.NonEmpty[B]
    def concat[B >: A](l2: BList[B]): BList.NonEmpty[B]
    def uncons: Some[(A, BList[A])]
    def headOption: Some[A]
    def tailOption: Some[BList[A]]
    def flatMapNonEmpty[B](fn: A => NonEmpty[B]): NonEmpty[B]
    final def ++[B >: A](l2: NonEmpty[B]): NonEmpty[B] = concat(l2)
  }

  object NonEmpty extends compat.BListCompatCompanion {
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
            case impl: Impl[A] =>
              impl.tailBList match {
                case Empty =>
                  Eval.now(arg0.map(traverseArray(impl.block.slice(impl.offset, BlockSize)).asInstanceOf[G[Array[B]]]) {
                    arrayB =>
                      Impl(impl.offset, new Array[Any](impl.offset) ++ arrayB, BList.empty)
                  })
                case nonemptytail: Impl[A] =>
                  arg0 match {
                    case x: StackSafeMonad[G] => // optimization described in issue #4480
                      // could traverse it as a chain and then just convert that to BList through iterator idk
                      // i wonder if this is faster because of all the conversions
                      // this implementation follows the one taken in #4498, building a Chain
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
            case impl: Impl[A] =>
              impl.tailBList match {
                case Empty                 => Eval.now(traverseBlockVoid(impl.offset, impl.block))
                case nonemptytail: Impl[A] =>
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

  private class Impl[+A](val offset: Int, val block: Array[A @uncheckedVariance], val tailBList: BList[A])
      extends NonEmpty[A] {

    override def equals(other: Any): Boolean =
      other match {
        case o: Impl[_] =>
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
        val ary = block.clone().asInstanceOf[Array[B]]
        val nextOffset = offset - 1
        ary(nextOffset) = a
        Impl(nextOffset, ary, tailBList)
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
    def get(idx: Long): Option[A] = {
      if (idx < 0) { None }
      else {
        @tailrec
        def go(idx: Long, l: BList[A]): Option[A] = {
          l match {
            case Empty                    => None
            case impl: Impl[A] @unchecked =>
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
          case Empty                    => throw new IndexOutOfBoundsException
          case impl: Impl[A] @unchecked =>
            if (idx < BlockSize - impl.offset) {
              impl.block(impl.offset + idx.toInt)
            } else {
              go(idx - (BlockSize - impl.offset), impl.tailBList)
            }
        }
      }
      go(idx, this)
    }
    def splitAt(idx: Int): (BList[A], BList[A]) = {
      def buildLists(idx: Int, l: BList[A]): Eval[(BList[A], BList[A])] = {
        l match {
          case Empty                    => Eval.now((Empty, Empty))
          case impl: Impl[A] @unchecked =>
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
              Eval.defer(buildLists(idx - (BlockSize - impl.offset), impl.tailBList)).map {
                _ match { case (l1_tail, l2) => (Impl(impl.offset, impl.block, l1_tail), l2) }
              }
            }
        }
      }
      buildLists(idx, this).value
    }
    def lastOption: Some[A] = {
      @tailrec
      def go(self: Impl[A]): Some[A] = {
        self.tailBList match {
          case Empty                    => Some(self.block(BlockSize - 1))
          case next: Impl[A] @unchecked => go(next)
        }
      }
      go(this)
    }
    def size: Long = {
      @tailrec
      def loop(l: BList[A], acc: Long): Long = {
        l match {
          case Empty                    => acc
          case impl: Impl[A] @unchecked => loop(impl.tailBList, acc + (BlockSize - impl.offset))
        }
      }
      loop(this, 0L)
    }
    // stack safe version
    def map[B](fn: A => B): BList.NonEmpty[B] = {
      val builder = List.newBuilder[B]
      @tailrec
      def loop(l: BList[A]): List[B] = {
        l match {
          case Empty         => builder.result()
          case impl: Impl[A] =>
            for (i <- impl.offset until BlockSize) {
              builder += fn(impl.block(i))
            }
            loop(impl.tailBList)
        }
      }
      fromList(loop(this)).asInstanceOf[NonEmpty[B]]
    }
    def filter(p: A => Boolean): BList[A] = {
      // this implementation does not condense blocks
      // maybe there could be a counter where if enough elements are dropped (proportion of total size/total number of nodes?) a condenser could be run on the resulting list
      def go(l: BList[A]): Eval[BList[A]] = l match {
        case Empty         => Eval.now(Empty)
        case impl: Impl[A] =>
          // "optimization" if block remains unchanged (this might not actually speed things up overall, but it skips allocations in this special case)
          if (impl.block.forall(p)) {
            return Eval.defer(go(impl.tailBList)).map(Impl(impl.offset, impl.block.asInstanceOf[Array[Any]], _))
          }

          var i = BlockSize - 1
          var offset_in_newblock = BlockSize
          val newblock = new Array[Any](
            BlockSize
          ) // there is at least one element removed, so we need to zero out arbitrarily bigger prefix
          // iterate backwards
          while (i >= impl.offset) {
            if (p(impl.block(i))) {
              offset_in_newblock -= 1
              newblock(offset_in_newblock) = impl.block(i)
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
    // this whole thing is just a copy paste of filter, maybe i should remove it
    def filterNot(p: A => Boolean): BList[A] = {
      def go(l: BList[A]): Eval[BList[A]] = l match {
        case Empty         => Eval.now(Empty)
        case impl: Impl[A] =>
          // i will not condense blocks. but maybe i could have a counter where if enough elements are dropped i could run a condenser on the resulting list automatically
          var i = BlockSize - 1
          var offset_in_newblock = BlockSize
          val newblock = new Array[Any](BlockSize)
          // iterate backwards
          while (i >= impl.offset) {
            if (!p(impl.block(i))) {
              offset_in_newblock -= 1
              newblock(offset_in_newblock) = impl.block(i)
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
    // adapted from scala List's implementation of collect https://github.com/scala/scala/blob/2.13.x/src/library/scala/collection/immutable/List.scala#L250
    private val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }
    def collect[B](pf: PartialFunction[A, B]): BList[B] = {
      def go(l: BList[A]): Eval[BList[B]] = l match {
        case Empty         => Eval.now(Empty)
        case impl: Impl[A] =>
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
          case Empty                    => acc
          case impl: Impl[A] @unchecked =>
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
          case Empty                    => init
          case impl: Impl[A] @unchecked =>
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
            case impl: Impl[A] @unchecked =>
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
          case impl: Impl[A] @unchecked =>
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
          case Empty         => Eval.now(Empty)
          case impl: Impl[A] =>
            if (n <= 0) {
              Eval.now(Empty)
            } else if (n >= BlockSize - impl.offset) {
              // Impl(offset, block, tailBList.take(n - (BlockSize - offset)))
              Eval.defer(go(n - (BlockSize - impl.offset), impl.tailBList)).map(Impl(impl.offset, impl.block, _))
            } else {
              val ary = new Array[Any](BlockSize)
              System.arraycopy(impl.block, impl.offset, ary, BlockSize - n, n) // safe conversion 0<n<BlockSize
              Eval.now(Impl(BlockSize - n, ary, Empty))
            }
        }

      go(m, this).value
    }

    def takeWhile(p: A => Boolean): BList[A] = {
      def go(l: BList[A]): Eval[BList[A]] = l match {
        case Empty         => Eval.now(Empty)
        case impl: Impl[A] =>
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
      def go(self: Impl[A]): Eval[BList.NonEmpty[B]] = {
        self.tailBList match {
          case Empty =>
            Eval.now(Impl(self.offset, self.block.asInstanceOf[Array[Any]], l2))
          case next: Impl[A] @unchecked =>
            Eval.defer(go(next)).map(Impl(self.offset, self.block.asInstanceOf[Array[Any]], _))
        }
      }

      // for now, the only optimization is checking if either are empty
      (this, l2) match {
        case (_, Empty) => this
        case (_, _)     => go(this).value
      }
    }

    override def toList: List[A] = {
      val builder = List.newBuilder[A]
      @tailrec
      def loop(l: BList[A]): List[A] =
        l match {
          case Empty                    => builder.result()
          case impl: Impl[A] @unchecked =>
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
          case impl: Impl[A] @unchecked =>
            loop(impl.tailBList, impl.block.slice(impl.offset, BlockSize).reverse.toList ::: acc)
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

    def iterator: Iterator[A] = new BListIterator(this)

    private[collections] def toStringInBlocks: String = {
      val strb = new java.lang.StringBuilder
      strb.append("BList(")
      @tailrec
      def loop(first: Boolean, l: BList[A]): Unit = {
        if (!first) strb.append(", "): Unit
        l match {
          case Empty                    => strb.append("Empty"): Unit
          case impl: Impl[A] @unchecked =>
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
    final private class BListIterator(var curNode: Impl[A @uncheckedVariance]) extends Iterator[A] {
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
              case impl: Impl[A] =>
                curOffset = impl.offset
                curNode = impl
              case Empty => // nothing, keep offset at blocksize
            }
          }
          next_elmt
        }
    }
    final private class BListSeq(private val xs: Impl[A @uncheckedVariance]) extends LinearSeq[A] {
      override def apply(i: Int): A = xs.getUnsafe(i.toLong)
      override def iterator: Iterator[A] = new BListIterator(xs)
      override def length: Int = xs.size.toInt // has to be an int so I cast
      override def head: A = xs.head
      override def tail: LinearSeq[A] = {
        xs.drop(1) match {
          case Empty           => LinearSeq.empty
          case tlimpl: Impl[A] => new BListSeq(tlimpl)
        }
      }
      override def isEmpty: Boolean = false
    }
  }

  def fromList[A](l: List[A]): BList[A] =
    fromListReverse(l.reverse)

  def fromListReverse[A](l: List[A]): BList[A] = {
    @tailrec
    def go(l: List[A], acc: BList[A]): BList[A] = {
      l match {
        case Nil    => acc
        case h :: t => go(t, acc.prepend(h))
      }
    }
    go(l, BList.empty)
  }

  // from is implemented in BListCompatCompanion because 2.12 does not support IterableOnce
  private[collections] def from_helper[A](iter: Iterator[A]): BList[A] = {
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

  def apply[A](elems: A*): BList[A] = {
    BList.from(elems)
  }

  def empty[A]: BList[A] = Empty

  // typeclasses stuff
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
          case Empty         => arg0.pure(BList.empty)
          case impl: Impl[A] =>
            // we use the nonempty implementatoin
            NonEmpty.catsCollectionNonEmptyBListInstances.nonEmptyTraverse(impl)(f)(arg0).asInstanceOf[G[BList[B]]]
        }
      override def traverseVoid[G[_], A, B](fa: BList[A])(f: A => G[B])(implicit arg0: Applicative[G]): G[Unit] = {
        fa match {
          case Empty         => arg0.unit
          case impl: Impl[A] =>
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
        val buf = List.newBuilder[B]
        @tailrec
        def go(lists: List[BList[Either[A, B]]]): Unit = lists match {
          case (impl: Impl[_]) :: tail =>
            // loop over block, progressing to next element when right is reached
            var curoffset = impl.offset
            var prefix: List[BList[Either[A, B]]] = List(
              impl.tailBList.asInstanceOf[BList[Either[A, B]]]
            ) // holds the prefix of the list that goes to the next rec call
            while (curoffset < BlockSize) {
              impl
                .block(curoffset)
                .asInstanceOf[Either[A, B]] match { // this is to prevent compiler warning about inexhuastive match
                case Right(b) =>
                  buf += b
                  curoffset += 1
                case Left(a_prime) =>
                  if (curoffset >= BlockSize - 1) {
                    // go(f(a_prime) :: impl.tailBList :: tail
                    prefix = List(f(a_prime), impl.tailBList.asInstanceOf[BList[Either[A, B]]])
                  } else {
                    // go(f(a_prime) :: Impl(curoffset+1, impl.block, impl.tailBList) :: tail)
                    prefix = List(f(a_prime),
                                  Impl(curoffset + 1, impl.block, impl.tailBList).asInstanceOf[BList[Either[A, B]]]
                    )
                  }
                  curoffset = BlockSize // to break out of loop
              }
            }
            go(prefix ++ tail)
          case Empty :: tail => go(tail)
          case Nil           => ()
        }
        go(f(a) :: Nil)
        BList.fromList(buf.result())
      }
    }
}
