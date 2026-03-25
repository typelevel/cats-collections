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

trait FastBlockedList[+T]{
    def uncons[A >: T]: Option[(A, FastBlockedList[A])]
    def prepend[A >: T](a: A): FastBlockedList[A]
    def forEach[U](f: T => U): Unit
    def foldLeft[B](start: B)(f: (B, T) => B): B
    def isEmpty: Boolean
  }

  object FastBlockedList {


    def apply[A](elements: A*)(BlockSize: Int): FastBlockedList[A] = {
      elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
    }
    def apply[A](elements: List[A])(BlockSize: Int): FastBlockedList[A] = {
      elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
    }
    def empty[A](BlockSize: Int): FastBlockedList[A] = Empty(BlockSize)



    private case class Empty(BlockSize: Int) extends FastBlockedList[Nothing] {

      override def uncons[A >: Nothing]: Option[(A, FastBlockedList[A])] = None

      override def prepend[A >: Nothing](a: A): FastBlockedList[A] = {
        val arrayBlock = new Array[Any](BlockSize)
        val offset = BlockSize - 1
        arrayBlock(offset) = a
        Impl(offset, arrayBlock, this, BlockSize)
      }

      override def isEmpty: Boolean = true

      override def forEach[U](f: Nothing => U): Unit = ()

      override def foldLeft[B](start: B)(f: (B, Nothing) => B): B = start
    }

    private case class Impl[+T](offset: Int, block: Array[Any], tail: FastBlockedList[T], BlockSize: Int) extends FastBlockedList[T] {
      @inline
      override def uncons[A >: T]: Option[(A, FastBlockedList[A])] = {

        val next = if (offset + 1 < BlockSize) {
          Impl(offset + 1, block, tail, BlockSize)
        } else {
          tail
        }
        Some((block(offset).asInstanceOf[A], next))
      }

      override def prepend[A >: T](a: A): FastBlockedList[A] = {
        if (offset > 0) {
          val nextOffset = offset - 1
          block(nextOffset) = a
          Impl(nextOffset, block, tail, BlockSize)
        } else {
          val newBlockedArray = new Array[Any](BlockSize)
          val newOffset = BlockSize - 1
          newBlockedArray(newOffset) = a
          Impl(newOffset, newBlockedArray, this, BlockSize)
        }
      }

      override def isEmpty: Boolean = false

      override def forEach[U](f: T => U): Unit = {
        var i = offset
        while (i < BlockSize){
          f(block(i).asInstanceOf[T])
          i += 1
        }
        tail.forEach(f)
      }

      override def foldLeft[B](start: B)(f: (B, T) => B): B = {
        var acc = start
        var i = offset
        while(i < BlockSize){
          acc = f(acc, block(i).asInstanceOf[T])
          i += 1
        }
        tail.foldLeft(acc)(f)
      }

    }
  }
