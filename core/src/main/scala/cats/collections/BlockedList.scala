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

import cats.Eval

import scala.annotation.tailrec

sealed trait BlockedList[+T] {
  def uncons[A >: T]: Option[(A, BlockedList[A])]
  def prepend[A >: T](a: A): BlockedList[A]
  def forEach[U](f: T => U): Unit
  def foldLeft[B](start: B)(f: (B, T) => B): B
  def map[B](f: T => B): BlockedList[B]
  def map2expirement[B](f: T => B): BlockedList[B]
  def reverse: BlockedList[T]
  def isEmpty: Boolean
}

object BlockedList {

  def apply[A](elements: A*)(BlockSize: Int): BlockedList[A] = {
    elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
  }
  def apply[A](elements: List[A])(BlockSize: Int): BlockedList[A] = {
    elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
  }
  def empty[A](BlockSize: Int): BlockedList[A] = Empty(BlockSize)

  private case class Empty(BlockSize: Int) extends BlockedList[Nothing] {

    override def uncons[A >: Nothing]: Option[(A, BlockedList[A])] = None

    override def prepend[A >: Nothing](a: A): BlockedList[A] = {
      val arrayBlock = new Array[Any](BlockSize)
      val offset = BlockSize - 1
      arrayBlock(offset) = a
      Impl(offset, arrayBlock, this, BlockSize)
    }

    override def isEmpty: Boolean = true

    override def forEach[U](f: Nothing => U): Unit = ()

    override def foldLeft[B](start: B)(f: (B, Nothing) => B): B = start

    override def map[B](f: Nothing => B): BlockedList[B] = this

    override def map2expirement[B](f: Nothing => B): BlockedList[B] = this

    override def reverse: BlockedList[Nothing] = this
  }

  private case class Impl[+T](offset: Int, block: Array[Any], tail: BlockedList[T], BlockSize: Int)
      extends BlockedList[T] {
    @inline
    override def uncons[A >: T]: Option[(A, BlockedList[A])] = {
      val next = if (offset + 1 < BlockSize) {
        Impl(offset + 1, block, tail, BlockSize)
      } else {
        tail
      }
      Some((block(offset).asInstanceOf[A], next))
    }

    override def prepend[A >: T](a: A): BlockedList[A] = {
      val newArray = new Array[Any](BlockSize)
      if (offset > 0) {
        System.arraycopy(block, offset, newArray, offset, BlockSize - offset)
        val nextOffset = offset - 1
        newArray(nextOffset) = a
        Impl(nextOffset, newArray, tail, BlockSize)
      } else {
        val newOffset = BlockSize - 1
        newArray(newOffset) = a
        Impl(newOffset, newArray, this, BlockSize)
      }
    }

    override def isEmpty: Boolean = false

    override def forEach[U](f: T => U): Unit = {
      @tailrec
      def helper(acc: BlockedList[T]): Unit = {
        acc match {
          case Empty(BlockSize)                     => ()
          case Impl(offset, block, tail, BlockSize) =>
            var i = offset
            while (i < BlockSize) {
              f(block(i).asInstanceOf[T])
              i += 1
            }
            helper(tail)
        }
      }
      helper(this)
    }

    override def foldLeft[B](start: B)(f: (B, T) => B): B = {

      @tailrec
      def helper(finalAcc: B, remainList: BlockedList[T]): B = {
        remainList match {
          case Empty(BlockSize)                     => finalAcc
          case Impl(offset, block, tail, BlockSize) =>
            var acc = finalAcc
            var i = offset
            while (i < BlockSize) {
              acc = f(acc, block(i).asInstanceOf[T])
              i += 1
            }
            helper(acc, tail)
        }
      }
      helper(start, this)
    }

    override def map[B](f: T => B): BlockedList[B] = {

      @tailrec
      def helper(curent: BlockedList[T], acc: BlockedList[B]): BlockedList[B] = {
        curent match {
          case Empty(BlockSize)                     => acc
          case Impl(offset, block, tail, BlockSize) =>
            val arrayCopy = new Array[Any](BlockSize)
            var i = offset
            while (i < BlockSize) {
              arrayCopy(i) = f(block(i).asInstanceOf[T])
              i += 1
            }
            helper(tail, Impl(offset, arrayCopy, acc, BlockSize))
        }
      }

      helper(this, Empty(BlockSize)).reverse

    }

    def map2expirement[B](f: T => B): BlockedList[B] = {
      def helper(curent: BlockedList[T], acc: BlockedList[B] => BlockedList[B]): BlockedList[B] = {
        curent match {
          case Empty(BlockSize) => acc(empty(BlockSize))

          case Impl(offset, block, tail, BlockSize) =>
            val arrayCopy = new Array[Any](BlockSize)
            var i = offset
            while (i < BlockSize) {
              arrayCopy(i) = f(block(i).asInstanceOf[T])
              i += 1
            }
            helper(tail, (rest: BlockedList[B]) => acc(Impl(offset, arrayCopy, rest, BlockSize)))
        }
      }
      helper(this, identity)
    }

    override def reverse: BlockedList[T] = {
      @tailrec
      def helper(curent: BlockedList[T], acc: BlockedList[T]): BlockedList[T] = {
        curent match {
          case Empty(BlockSize)                     => acc
          case Impl(offset, block, tail, BlockSize) =>
            helper(tail, Impl(offset, block, acc, BlockSize))
        }
      }
      helper(this, Empty(BlockSize))
    }

  }
}
