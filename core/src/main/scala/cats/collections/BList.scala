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

import scala.annotation.tailrec

sealed abstract class BList[+A] {
  def uncons: Option[(A, BList[A])]
  def prepend[B >: A](a: B): BList.NonEmpty[B]
  def headOption: Option[A]
  def tailOption: Option[BList[A]]
  def get(idx: Long): Option[A]
  def getUnsafe(idx: Long): A
  def lastOption: Option[A] // a bit more efficient than get(size-1)
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

  override def toString: String = { // go back and optimize with blocks
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
    // def unapply[A](l: Empty.type): None.type = None
    def uncons = None
    def prepend[B >: Nothing](a: B): BList.NonEmpty[B] = { // why would we put the element at the end of a block? dont we want to fill from start out. in this case we would want head and tail pointer
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
    // TODO can put methods in here that are only safe for nonempty (ex. head, reduce)
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

  // (maybe impl will be covariant or not)
  private class Impl[A](val offset: Int, val block: Array[A], val tailBList: BList[A]) extends NonEmpty[A] {

    override def equals(other: Any): Boolean = {
      // helper to get next element of BList
      def next[B](node: Impl[B], curoffset: Int): Option[(B, Impl[B], Int)] = {
        if (curoffset < BlockSize - 1) {
          Some((node.block(curoffset + 1), node, curoffset + 1))
        } else {
          // we need to see if the tail is empty and try to take an element from it
          node.tailBList match {
            case Empty                          => None
            case Impl(tailoffset, tailblock, _) => // there will be at least one element in every block
              Some((tailblock(tailoffset), node.tailBList.asInstanceOf[Impl[B]], tailoffset + 1))
          }
        }
      }

      other match {
        case that: Impl[_] =>
          // 3-tuple has structure: current element, current node (nonempty), current offset in to curnode's block
          @tailrec
          def loop[B](list1: (A, Impl[A], Int), list2: (B, Impl[B], Int)): Boolean = {
            if (list1._1 != list2._1) false

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

    override def hashCode(): Int = {
      // determined by only the valid elements in the first block
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
        val ary = new Array[Any](BlockSize) // need a blank one for a new block
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
            case Empty                             => None
            case Impl(offset1, block1, tailBList1) =>
              if (idx < BlockSize - offset1) {
                Some(block1(offset1 + idx.toInt))
              } else {
                go(idx - (BlockSize - offset1), tailBList1)
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
          case Empty                             => throw new IndexOutOfBoundsException
          case Impl(offset1, block1, tailBList1) =>
            if (idx < BlockSize - offset1) {
              block1(offset1 + idx.toInt)
            } else {
              go(idx - (BlockSize - offset1), tailBList1)
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
          case Empty                        => acc
          case Impl(offset1, _, tailBList1) => loop(tailBList1, acc + (BlockSize - offset1))
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
          case Empty                             => acc
          case Impl(offset1, block1, tailBList1) =>
            var newacc = acc
            var i = offset1
            while (i < block1.length) {
              newacc = fn(newacc, block1(i))
              i += 1
            }
            loop(newacc, tailBList1)
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
            case Impl(offset1, block1, tailBList1) =>
              if (n >= BlockSize - offset1) {
                go(n - (BlockSize - offset1), tailBList1)
              } else {
                val ary = new Array[Any](BlockSize)
                val newOffset: Int = offset1 + n.asInstanceOf[Int] // type conversion safe because 0<n<BlockSize
                System.arraycopy(block1, newOffset, ary, newOffset, BlockSize - newOffset)
                Impl(newOffset, ary, tailBList1)
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
          // case Impl(_,_,_) => Impl(self.offset, self.block.asInstanceOf[Array[B]], go(self.tailBList.asInstanceOf[Impl[A]]))
        }
      }

      // for now only optimization is checking if either are empty
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
          case Empty                             => builder.result()
          case Impl(offset1, block1, tailBList1) =>
            // append valid things in the block to acc
            for (i <- offset1 until BlockSize) {
              builder += block1(i)
            }
            loop(tailBList1)
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
          case Empty                             => strb.append("Empty"): Unit
          case Impl(offset1, block1, tailBList1) =>
            strb.append("Block(")
            strb.append(block1(offset1).toString)
            for (i <- offset1 + 1 until BlockSize) {
              strb.append(", ")
              strb.append(block1(i).toString)
            }
            strb.append(")")
            loop(false, tailBList1)
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
}
