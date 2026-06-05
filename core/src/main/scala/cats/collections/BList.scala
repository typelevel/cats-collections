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
  def combineK[B >: A](l2: BList[B]): BList[B]
  def toList: List[A]
  // def strictFoldRight[B](fin: B)(fn: (A, B) => B): B

  // for development and testing
  def toStringInBlocks: String

  final def ::[B >: A](a: B): BList[B] = prepend(a)

  final def ++[B >: A](l2: BList[B]): BList[B] = combineK(l2)

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
  private val BlockSize = 4 // test with different values

  case object Empty extends BList[Nothing] {
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
    def combineK[B](l2: BList[B]): BList[B] = l2
    override def toList: List[Nothing] = Nil
    def toStringInBlocks: String = "Empty"
    // def strictFoldRight[B](acc: B)(fn: (Nothing, B) => B): B = acc

  }
  sealed abstract class NonEmpty[+A] extends BList[A] {
    // TODO can put methods in here that are only safe for nonempty (ex. head, reduce)
    def head: A
    def tail: BList[A]
    // def reduce
  }

  object NonEmpty {
    def apply[A](h: A, t: BList[A]): NonEmpty[A] =
      t.prepend(h) // maybe make this take arbitary number of args of same type and make a list out of them?
    def unapply[A](l: BList[A]): Option[(A, BList[A])] = l.uncons
  }

  private object Impl {
    def apply[A](offset: Int, block: Array[A], tailBList: BList[A]): Impl[A] =
      new Impl(offset, block, tailBList)
    def apply[A](offset: Int, block: Array[Any], tailBList: BList[A]): Impl[A] =
      new Impl(offset, block.asInstanceOf[Array[A]], tailBList)
  }

  // (maybe impl will be covariant or not)
  private case class Impl[A](offset: Int, block: Array[A], tailBList: BList[A]) extends NonEmpty[A] {
    def uncons = {
      val nextOffset = offset + 1
      val next = if (nextOffset == block.length) tailBList else Impl(nextOffset, block, tailBList)
      Some((block(offset), next))
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
        Impl(offset + 1, block, tailBList)
      } else {
        tailBList
      }
    }
    def headOption: Option[A] = {
      Some(block(offset))
    }
    def tailOption: Option[BList[A]] = {
      Some(tail)
    }
    def get(idx: Long): Option[A] = {
      if (idx < 0) { None }
      else {
        @tailrec
        def go(idx: Long, l: BList[A]): Option[A] = {
          l match {
            case Empty                          => None
            case Impl(offset, block, tailBList) =>
              if (idx < BlockSize - offset) {
                Some(block(offset + idx.toInt))
              } else {
                go(idx - (BlockSize - offset), tailBList)
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
          case Empty                          => throw new IndexOutOfBoundsException
          case Impl(offset, block, tailBList) =>
            if (idx < BlockSize - offset) {
              block(offset + idx.toInt)
            } else {
              go(idx - (BlockSize - offset), tailBList)
            }
        }
      }
      go(idx, this)
    }
    def lastOption: Option[A] = {
      @tailrec
      def go(l: BList[A]): Option[A] = {
        l.asInstanceOf[Impl[A]] match {
          case Impl(_, block, tailBList) =>
            tailBList match {
              case Empty         => Some(block(BlockSize - 1))
              case Impl(_, _, _) => go(tailBList)
            }
        }
      }
      go(this)
    }
    def size: Long = {
      @tailrec
      def loop(l: BList[A], acc: Long): Long = {
        l match {
          case Empty                      => acc
          case Impl(offset, _, tailBList) => loop(tailBList, acc + (BlockSize - offset))
        }
      }
      loop(this, 0L)
    }
    def map[B](fn: A => B): BList[B] = {
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
          case Empty                          => acc
          case Impl(offset, block, tailBList) =>
            var newacc = acc
            var i = offset
            while (i < block.length) {
              newacc = fn(newacc, block(i))
              i += 1
            }
            loop(newacc, tailBList)
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
            case Impl(offset, block, tailBList) =>
              if (n >= BlockSize - offset) {
                go(n - (BlockSize - offset), tailBList)
              } else {
                val ary = new Array[Any](BlockSize)
                val newOffset: Int = offset + n.asInstanceOf[Int] // type conversion safe because 0<n<BlockSize
                System.arraycopy(block, newOffset, ary, newOffset, BlockSize - newOffset)
                Impl(newOffset, ary, tailBList)
              }

          }
        }
      }
      go(n, this)
    }
    def combineK[B >: A](l2: BList[B]): BList[B] = {
      // TODO add special cases for if the block is a certain amount empty to shuffle things over
      // maybe use benchmarking to find out optimal number here??

      // @tailrec   !!! not tailBListrec rn, could use cps but maybe ask first !!!
      def go(l: BList[A]): BList[B] = {
        l.asInstanceOf[Impl[A]] match {
          case Impl(offset, block, tailBList) =>
            tailBList match {
              case Empty => Impl(offset, block.asInstanceOf[Array[B]], l2) // and the current block!!!
              case Impl(_,
                        _,
                        _
                  ) => // maybe i shouldnt copy the block because no changes are occuring. any operation that needs to change blocks will copy, so realistically two lists can share an array if they both never mutate it
                Impl(offset, block.asInstanceOf[Array[B]], go(tailBList))
            }
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
          case Empty                          => builder.result()
          case Impl(offset, block, tailBList) =>
            // append valid things in the block to acc
            for (i <- offset until BlockSize) {
              builder += block(i)
            }
            loop(tailBList)
        }
      loop(this)
    }

    def toStringInBlocks: String = {
      val strb = new java.lang.StringBuilder
      strb.append("BList(")
      @tailrec
      def loop(first: Boolean, l: BList[A]): Unit = {
        if (!first) strb.append(", ")
        l match {
          case Empty                          => strb.append("Empty"): Unit
          case Impl(offset, block, tailBList) =>
            strb.append("Block(")
            strb.append(block(offset).toString)
            for (i <- offset + 1 until BlockSize) {
              strb.append(", ")
              strb.append(block(i).toString)
            }
            strb.append(")")
            loop(false, tailBList)
        }
      }

      loop(true, this)
      strb.append(")")
      strb.toString
    }

    /*
    def strictFoldRight[B](acc: B)(fn: (A, B) => B): B = {
      // TODO
      val fnblock = (block:Array[Any],offset:Int,acc1:B ) => {
          // get only the useful part of the block
          val ary : Array[A] = block.drop(offset).map(_.asInstanceOf[A])
          ary.foldRight(acc1)(fn) //fold right on the block to produce new accumulator
        }
      
      def go(l:BList[A],acc: B,fnblock: (Array[Any], Int, B) => B): B = {
        l match {
          case Empty => acc
          case Impl(offset,block, tailBList)=> fnblock(block,offset, go(tailBList, acc, fnblock))
        }
      }
      go(this, acc, fnblock)
    }
     */

  }

  def empty[A]: BList[A] = Empty
}
