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

import cats.{Alternative, Applicative, Eq, Eval, Foldable, Functor, Monad, MonoidK, SemigroupK, Traverse}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3.seqSeed
import org.typelevel.scalaccompat.annotation._
//import cats.syntax.all._

sealed abstract class BList[+A] {
  def uncons: Option[(A, BList[A])]
  def prepend[B >: A](a: B): BList.NonEmpty[B]
  def headOption: Option[A]
  def tailOption: Option[BList[A]]
  def get(idx: Long): Option[A]
  def getUnsafe(idx: Long): A
  def lastOption: Option[A]
  def size: Long
  def map[B](fn: A => B): BList[B]
  def foldLeft[B](init: B)(fn: (B, A) => B): B
  def drop(n: Long): BList[A]
  def concat[B >: A](l2: BList[B]): BList[B]
  def toList: List[A]
  def toListReverse: List[A]
  def isEmpty: Boolean
  def flatMap[B](fn: A => BList[B]): BList[B]

  final def ::[B >: A](a: B): BList[B] = prepend(a)
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
  final private[collections] val BlockSize = 4 // test with different values

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
    def lastOption: None.type = None
    def size: Long = 0
    def map[B](fn: Nothing => B): BList[B] = Empty
    def foldLeft[B](acc: B)(fn: (B, Nothing) => B): B = acc
    def drop(n: Long): BList[Nothing] = Empty
    def concat[B](l2: BList[B]): BList[B] = l2
    override def toList: List[Nothing] = Nil
    override def toListReverse: List[Nothing] = Nil
    def isEmpty: Boolean = true
    def flatMap[B](fn: Nothing => BList[B]): BList[B] = Empty

    private[collections] def toStringInBlocks: String = "Empty"

    override def equals(other: Any): Boolean = other match {
      case _: Empty.type => true
      case _             => false
    }

    override def hashCode(): Int = finalizeHash(seqSeed, 0)

  }
  sealed abstract class NonEmpty[+A] extends BList[A] {
    def head: A
    def tail: BList[A]
    def map[B](fn: A => B): BList.NonEmpty[B]
    def concat[B >: A](l2: BList[B]): BList.NonEmpty[B]
    def uncons: Some[(A, BList[A])]
    def headOption: Some[A]
    def tailOption: Some[BList[A]]
  }

  object NonEmpty {
    def apply[A](h: A, t: BList[A]): NonEmpty[A] =
      t.prepend(h)
    def unapply[A](l: NonEmpty[A]): Some[(A, BList[A])] =
      l.uncons
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

    // equals version 2
    override def equals(other: Any): Boolean = {
      // Helper recursive class-level function. start inclusive, end exclusive
      // ary1 will be shorter or equal length to ary2
      def arrayEqualsPrefix(ary1: Array[_], ary2: Array[_], ary1start: Int, ary2start: Int): Boolean = {
        // compare from ary1start until BlockSize with other ary from ary2start
        var i = 0
        while (i + ary1start < BlockSize) {
          if (ary1(i + ary1start) != ary2(i + ary2start)) return false
          i += 1
        }
        true
      }

      other match {
        case that: Impl[_] =>
          // we know both lists are Impl here.
          var list1: Impl[_] = this
          var list2: Impl[_] = that
          var list1idx = list1.offset
          var list2idx = list2.offset

          while (!list1.isEmpty && !list2.isEmpty) {
            if (list1idx == list2idx) {
              if (!arrayEqualsPrefix(list1.block, list2.block, list1idx, list2idx))
                return false // TODO if this is false return false
              if (list1.tailBList.isEmpty || list2.tailBList.isEmpty) {
                return list1.tailBList.isEmpty && list2.tailBList.isEmpty
              }
              list1 = list1.tailBList.asInstanceOf[Impl[_]]
              list2 = list2.tailBList.asInstanceOf[Impl[_]]
              list1idx = list1.offset
              list2idx = list2.offset
            } else if (list1idx > list2idx) {
              if ((list1.tailBList.isEmpty) || !arrayEqualsPrefix(list1.block, list2.block, list1idx, list2idx))
                return false
              list2idx = list2idx + (BlockSize - list1idx)
              list1 = list1.tailBList.asInstanceOf[Impl[_]]
              list1idx = list1.offset
            } else { // (list1idx < list2idx)
              if ((list2.tailBList.isEmpty) || !arrayEqualsPrefix(list2.block, list1.block, list2idx, list1idx))
                return false
              list1idx = list1idx + (BlockSize - list2idx)
              list2 = list2.tailBList.asInstanceOf[Impl[_]]
              list2idx = list2.offset
            }

          }
          true

        case _ => false
      }
    }
    /*
    //equals version 1 - i think i will try to test the performance of the two versions against each other
    override def equals(other: Any): Boolean = {
      // helper to get next element of BList
      def next[B](node: Impl[B], curoffset: Int): Option[(B, Impl[B], Int)] = {
        if (curoffset < BlockSize - 1) {
          Some((node.block(curoffset + 1), node, curoffset + 1))
        } else {
          node.tailBList match {
            case _: Empty.type                   => None
            case tail: Impl[B] @unchecked => // there will be at least one element in every block
              Some((tail.block(tail.offset), tail, tail.offset))
          }
        }
      }

      other match {
        case that: Impl[_] =>
          // 3-tuple has structure: element, node where it is found, offset at which it was found
          @tailrec
          def loop[B](list1: (A, Impl[A], Int), list2: (B, Impl[B], Int)): Boolean = {
            if (list1._1 != list2._1) return false

            (next(list1._2, list1._3), next(list2._2, list2._3)) match {
              case (None, None)                         => true
              case (None, _) | (_, None)                => false
              case (Some(list1_tail), Some(list2_tail)) =>
                loop(list1_tail, list2_tail)
            }
            
          }

          loop(
            (this.block(this.offset), this, this.offset), // list1
            (that.block(that.offset), that, that.offset) // list2
          )

        case _ => false
      }
    }
     */

    /*
     * HashCode computation for BList is adapted from https://github.com/scala/scala/blob/e48c7888a0ae08b8d273f03bfc62452fc2ed886d/src/library/scala/util/hashing/MurmurHash3.scala
     * such that BList hashcodes are consistent with other list-like datatypes in the Scala standard library.
     */
    override def hashCode(): Int = {
      var n = 0
      var h = seqSeed
      var rangeState = 0 // 0 = no data, 1 = first elem read, 2 = has valid diff, 3 = invalid
      var rangeDiff = 0
      var prev = 0
      var initial = 0
      var curoffset = this.offset
      var elems: Impl[_] = this

      var elemsIsNotEmpty = true
      while (elemsIsNotEmpty) { // change this check to be later i think
        val head = elems.block(curoffset)
        val hash = head.##
        h = mix(h, hash)
        rangeState match {
          case 0 =>
            initial = hash
            rangeState = 1
          case 1 =>
            rangeDiff = hash - prev
            rangeState = 2
          case 2 =>
            if (rangeDiff != hash - prev) rangeState = 3
          case _ =>
        }
        prev = hash
        n += 1
        curoffset += 1

        // logic for proceeding to next block
        if (curoffset >= BlockSize) {
          elems.tailBList match {
            case Empty                         => elemsIsNotEmpty = false // break loop
            case nextBlock: Impl[_] @unchecked =>
              elems = nextBlock
              curoffset = nextBlock.offset
          }
        }
      }
      if (rangeState == 2) rangeHash(initial, rangeDiff, prev, seqSeed)
      else finalizeHash(h, n)
    }

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
    def map[B](fn: A => B): BList.NonEmpty[B] = {
      val ary = new Array[Any](BlockSize)
      var i = offset
      while (i < BlockSize) {
        ary(i) = fn(block(i))
        i += 1
      }
      Impl(offset, ary, tailBList.map(fn))
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
    def drop(n: Long): BList[A] = {
      @tailrec
      def go(n: Long, l: BList[A]): BList[A] = {
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
                val newOffset: Int = impl.offset + n.asInstanceOf[Int] // type conversion safe because 0<n<BlockSize
                System.arraycopy(impl.block, newOffset, ary, newOffset, BlockSize - newOffset)
                Impl(newOffset, ary, impl.tailBList)
              }

          }
        }
      }
      go(n, this)
    }
    def concat[B >: A](l2: BList[B]): BList.NonEmpty[B] = {
      // not tail rec
      def go(self: Impl[A]): BList.NonEmpty[B] = {
        self.tailBList match {
          case Empty                    => Impl(self.offset, self.block.asInstanceOf[Array[B]], l2)
          case next: Impl[A] @unchecked => Impl(self.offset, self.block.asInstanceOf[Array[B]], go(next))
        }
      }

      // for now, the only optimization is checking if either are empty
      (this, l2) match {
        case (_, Empty) => this
        case (_, _)     => go(this)
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

  def empty[A]: BList[A] = Empty

  /*
   * The following five methods are copied from https://github.com/scala/scala/blob/e48c7888a0ae08b8d273f03bfc62452fc2ed886d/src/library/scala/util/hashing/MurmurHash3.scala
   * They are helper functions for computing a hashcode for BList that is consistent with that of other
   * list-like datatypes in the Scala standard library (List, Vector, Range etc.)
   */
  final private def finalizeHash(hash: Int, length: Int): Int = avalanche(hash ^ length)
  final private def avalanche(hash: Int): Int = {
    var h = hash
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }
  final private def mixLast(hash: Int, data: Int): Int = {
    var k = data

    k *= 0xcc9e2d51
    k = java.lang.Integer.rotateLeft(k, 15)
    k *= 0x1b873593

    hash ^ k
  }
  final private def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = java.lang.Integer.rotateLeft(h, 13)
    h * 5 + 0xe6546b64
  }
  // this one isn't available in scala 2.12, so it is included in this file
  final private def rangeHash(start: Int, step: Int, last: Int, seed: Int): Int =
    avalanche(mix(mix(mix(seed, start), step), last))

  // typeclasses stuff
  implicit def eqBList[B]: Eq[BList[B]] =
    new Eq[BList[B]] {
      def eqv(xs: BList[B], ys: BList[B]): Boolean = xs.equals(ys)
    }
  implicit def catsCollectionBListFunctor: Functor[BList] =
    new Functor[BList] {
      override def map[B, C](a: BList[B])(f: B => C): BList[C] = a.map(f)
    }
  implicit def catsCollectionBListSemigroupK: SemigroupK[BList] =
    new SemigroupK[BList] {
      override def combineK[B](x: BList[B], y: BList[B]): BList[B] = x.concat(y)
    }
  implicit val catsCollectionsBListFoldable: Foldable[BList] =
    new Foldable[BList] {
      def foldLeft[A, B](xs: BList[A], init: B)(f: (B, A) => B): B =
        xs.foldLeft(init)(f)
      def foldRight[A, B](xs: BList[A], init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = xs match {
        case Empty                    => init
        case impl: Impl[A] @unchecked =>
          def loop(idx: Int): Eval[B] =
            if (idx >= BlockSize) Eval.defer(foldRight(impl.tailBList, init)(f))
            else Eval.defer(f(impl.block(idx), loop(idx + 1)))
          loop(impl.offset)
      }
      override def isEmpty[A](xs: BList[A]): Boolean = xs.isEmpty

      override def nonEmpty[A](xs: BList[A]): Boolean = !xs.isEmpty

      override def size[A](xs: BList[A]): Long = xs.size
    }
  implicit def catsCollectionBListApplicative: Applicative[BList] =
    new Applicative[BList] {
      // cartesian product
      def ap[A, B](ff: BList[(A) => B])(xs: BList[A]): BList[B] =
        ff.flatMap(f => xs.map(a => f(a)))
      def pure[A](x: A): BList[A] = Empty.prepend(x)
    }
  implicit def catsCollectionBListMonoidK: MonoidK[BList] =
    new MonoidK[BList] {
      def combineK[A](x: BList[A], y: BList[A]): BList[A] = catsCollectionBListSemigroupK.combineK(x, y)
      def empty[A]: BList[A] = Empty
    }

  @nowarn213(
    "msg=Calls to parameterless method compose will be easy to mistake for calls to overloads which have a single implicit parameter list"
  )
  implicit val catsCollectionBListInstances: Traverse[BList] with Alternative[BList]
  // with Monad[BList]
  =
    new Traverse[BList]
      with Alternative[BList]
      // with Monad[BList]
      {
      override def foldLeft[A, B](xs: BList[A], init: B)(f: (B, A) => B): B =
        catsCollectionsBListFoldable.foldLeft(xs, init)(f)
      override def foldRight[A, B](xs: BList[A], init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        catsCollectionsBListFoldable.foldRight(xs, init)(f)

      override def traverse[G[_], A, B](fa: BList[A])(f: (A) => G[B])(implicit arg0: Applicative[G]): G[BList[B]] = {
        def traverseArray(fa: Array[A])(f: (A) => G[B])(implicit arg0: Applicative[G]): G[Array[Any]] = {
          fa.foldLeft(arg0.pure(Array[Any]())) { (arrEffect, head) =>
            arg0.map2(arrEffect, f(head)) { (arr, b) =>
              arr :+ b
            }
          }
        }
        fa match {
          case Empty         => arg0.pure(BList.empty)
          case impl: Impl[A] =>
            arg0.map2(
              traverseArray(impl.block.slice(impl.offset, BlockSize))(f).asInstanceOf[G[Array[B]]],
              traverse(impl.tailBList)(f)
            ) { (arrayB, tailBList) =>
              Impl(impl.offset, new Array[Any](impl.offset) ++ arrayB, tailBList)
            }
        }
      }
      override def ap[A, B](ff: BList[(A) => B])(fa: BList[A]): BList[B] =
        catsCollectionBListApplicative.ap(ff)(fa)
      override def empty[A]: BList[A] = catsCollectionBListMonoidK.empty
      override def pure[A](x: A): BList[A] = catsCollectionBListApplicative.pure(x)
      override def combineK[A](x: BList[A], y: BList[A]): BList[A] = catsCollectionBListSemigroupK.combineK(x, y)
      // override def flatMap[A, B](fa: BList[A])(f: (A) => BList[B]): BList[B] =
      // TODO I already have a flatmap implementation !!!!
      //   fa match {
      //     case Empty => BList.empty
      //     case impl: Impl[_] =>
      //       //make a new block with impl.block f applied to all of it
      //       val ary = new Array[Any](BlockSize)
      //       //loop over block
      //       for (i <- impl.offset until BlockSize) {
      //         ary(i) = f (impl.block(i))
      //       }
      //       Impl(impl.offset, ary, flatMap(impl.tailBList)(f))

      //   }

      // override def tailRecM[A, B](a: A)(f: (A) => BList[Either[A, B]]): BList[B] =
      //   f(a) match {
      //     case Empty => Empty
      //     case impl : Impl[_] =>

      //     // todo maybe i have to check element wise??? check tree list's implementation
      //   }

    }

}
