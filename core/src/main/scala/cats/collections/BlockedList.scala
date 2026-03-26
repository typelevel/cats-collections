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

trait BlockedList[+T] {
  def uncons[A >: T]: Option[(A, BlockedList[A])]
  def prepend[A >: T](a: A): BlockedList[A]
  def forEach[U](f: T => U): Unit
  def foldLeft[B](start: B)(f: (B, T) => B): B
  def map[B](f: T => B): BlockedList[B]
  def map2expirement[B](f: T => B): BlockedList[B]
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
      var i = offset
      while (i < BlockSize) {
        f(block(i).asInstanceOf[T])
        i += 1
      }
      tail.forEach(f)
    }

    override def foldLeft[B](start: B)(f: (B, T) => B): B = {
      var acc = start
      var i = offset
      while (i < BlockSize) {
        acc = f(acc, block(i).asInstanceOf[T])
        i += 1
      }
      tail.foldLeft(acc)(f)
    }

    override def map[B](f: T => B): BlockedList[B] = {
      val arrayCopy = new Array[Any](BlockSize)
      var i = offset
      while (i < BlockSize) {
        arrayCopy(i) = f(block(i).asInstanceOf[T])
        i += 1
      }
      Impl(offset, arrayCopy, tail.map(f), BlockSize)
    }

    def map2expirement[B](f: T => B): BlockedList[B] = {
      this.foldLeft(empty[B](BlockSize))((acc, element) => acc.prepend(f(element)))
    }

  }
}
