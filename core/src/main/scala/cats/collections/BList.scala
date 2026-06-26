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
import scala.util.hashing.MurmurHash3
import org.typelevel.scalaccompat.annotation._

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
  def toIterator: Iterator[A]

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

    def toIterator: Iterator[Nothing] = new BListIterator(this)
    private[collections] def toStringInBlocks: String = "Empty"

    override def equals(other: Any): Boolean = other match {
      case _: Empty.type => true
      case _             => false
    }

    override def hashCode(): Int = Empty.toIterator.##

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

    override def equals(other: Any): Boolean =
      other match {
        case o: Impl[_] =>
          val iterX = this.toIterator
          val iterY = o.toIterator
          while (iterX.hasNext && iterY.hasNext) {
            if (iterX.next() != iterY.next()) { // not sure if i could be comparing with != here...
              return false
            }
          }
          iterX.hasNext == iterY.hasNext
        case _ => false
      }

    override def hashCode: Int =
      MurmurHash3.orderedHash(this.toIterator)

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
    def toIterator: Iterator[A] = new BListIterator(this)

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

  final class BListIterator[A](from: BList[A]) extends Iterator[A] {
    private var curNode: BList[A] = from
    private var curOffset: Int = if (!from.isEmpty) curNode.asInstanceOf[Impl[A]].offset else BlockSize

    def hasNext: Boolean = !curNode.isEmpty
    def next(): A =
      curNode match {
        case Empty         => throw new NoSuchElementException
        case impl: Impl[A] => {
          val next = impl.block(curOffset)
          curOffset += 1
          if (curOffset >= BlockSize) {
            // advance to next block
            curNode = impl.tailBList
            curOffset = curNode match {
              case Empty               => BlockSize
              case impl_prime: Impl[A] => impl_prime.offset
            }
          }
          next
        }
      }

  }
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
  implicit val catsCollectionBListInstances: Traverse[BList] with Alternative[BList] with Monad[BList] =
    new Traverse[BList] with Alternative[BList] with Monad[BList]{
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
      override def flatMap[A, B](fa: BList[A])(f: (A) => BList[B]): BList[B] = fa.flatMap(f)
      // adapted from the implementation of tailRecM for ListInstances 
      // https://github.com/typelevel/cats/blob/v0.7.0/core/src/main/scala/cats/instances/list.scala#L29
      override def tailRecM[A, B](a: A)(f: (A) => BList[Either[A, B]]): BList[B] = {
        // start by building a list then at the end up to fromList
        val buf = List.newBuilder[B]

        //@tailrec 
        def go(lists: List[BList[Either[A, B]]]): Unit = lists match {
          case Empty :: tail => go(tail)
          case (impl:Impl[Either[A, B]]) :: tail  => 
            //@tailrec
            // todo need to write this as a while loop since scala cant tail call optimization for mutual recursion
            def loopoverblock(offset:Int, block:Array[Either[A, B]], tailBList:BList[Either[A, B]]):Unit = {
              if (offset >= BlockSize) return go(tailBList :: tail)
              block(offset) match {
                case Right(b) => buf += b; loopoverblock(offset +1, block, tailBList)
                case Left(a_prime) => 
                  if ( offset >= BlockSize-1){
                    go((f(a_prime)) :: tailBList :: tail)
                  } else {
                    go((f(a_prime)) :: Impl(offset+1, block, tailBList ) :: tail)
                  }
              }
            }
            loopoverblock(impl.offset, impl.block, impl.tailBList)
          case Nil => ()
        }
        go(f(a) :: Nil)
        BList.fromList(buf.result())
      }

    }

}
