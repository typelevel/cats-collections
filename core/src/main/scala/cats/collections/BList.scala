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

import cats.{Eq, Eval, Foldable, Functor, SemigroupK}

import scala.annotation.tailrec
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
  def isEmpty: Boolean

  // for development and testing
  private[collections] def toStringInBlocks: String

  final def ::[B >: A](a: B): BList[B] = prepend(a)

  final def ++[B >: A](l2: BList[B]): BList[B] = concat(l2)

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
    def isEmpty: Boolean = true
    private[collections] def toStringInBlocks: String = "Empty"

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

  private class Impl[A](val offset: Int, val block: Array[A], val tailBList: BList[A]) extends NonEmpty[A] {

    override def equals(other: Any): Boolean = {
      // helper to get next element of BList
      def next[B](node: Impl[B], curoffset: Int): Option[(B, Impl[B], Int)] = {
        if (curoffset < BlockSize - 1) {
          Some((node.block(curoffset + 1), node, curoffset + 1))
        } else {
          node.tailBList match {
            case Empty                    => None
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
            if (list1._1 != list2._1) {
              false
            } else {

              (next(list1._2, list1._3), next(list2._2, list2._3)) match {
                case (None, None)                         => true
                case (None, _) | (_, None)                => false
                case (Some(list1_tail), Some(list2_tail)) =>
                  loop(list1_tail, list2_tail)
              }

            }

          }

          loop(
            (this.block(this.offset), this, this.offset), // list1
            (that.block(that.offset), that, that.offset) // list2
          )

        case _ => false
      }
    }

    override def hashCode(): Int = {
      // uses only the valid elements in the first block
      var res = 31
      for (i <- this.offset until BlockSize) {
        res = res * 31 + this.block(i).hashCode()
      }
      res
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
      while (i < ary.length) {
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
    def isEmpty: Boolean = false

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
    go(l, empty)
  }

  def empty[A]: BList[A] = Empty

  // typeclasses stuff
  implicit def eqBList[A: Eq]: Eq[BList[A]] =
    new Eq[BList[A]] {
      def eqv(xs: BList[A], ys: BList[A]): Boolean = xs == ys
    }
  implicit def catsCollectionBListFunctor[A]: Functor[BList] =
    new Functor[BList] {
      override def map[A, B](a: BList[A])(f: A => B): BList[B] = a.map(f)
    }
  implicit def catsCollectionBListSemigroupK[A]: SemigroupK[BList] =
    new SemigroupK[BList] {
      override def combineK[A](x: BList[A], y: BList[A]): BList[A] = x.concat(y)
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
}
