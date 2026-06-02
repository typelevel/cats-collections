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
  def combineK [B >: A](l2: BList[B]): BList[B]
  def toList: List[A]
  //def strictFoldRight[B](fin: B)(fn: (A, B) => B): B
  //final def take(n: Long): TreeList[A] =

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
    def prepend[A](a: A): BList[A] = { // why would we put the element at the end of a block? dont we want to fill from start out. in this case we would want head and tail pointer
      val ary = new Array[Any](BlockSize)
      val offset = BlockSize - 1
      ary(offset) = a
      Impl(offset, ary, Empty)
    }
    def headOption: None.type = None
    def tailOption: None.type = None
    def get(idx: Long): Option[Nothing] = None
    def getUnsafe(idx: Long): Nothing = throw new NoSuchElementException("invalid index")
    def lastOption: Option[Nothing] = None
    def size: Long = 0
    def map[B](fn: Nothing => B): BList[B] = Empty
    def foldLeft[B](acc: B)(fn: (B, Nothing) => B): B = acc
    def drop(n: Long): BList[Nothing] = Empty
    def combineK[B](l2: BList[B]): BList[B] = l2
    def toList: List[Nothing] = Nil
    // def strictFoldRight[B](acc: B)(fn: (Nothing, B) => B): B = acc

  }
  sealed abstract class NonEmpty[+A] extends BList[A] {
    // TODO can put methods in here that are only safe for nonempty (ex. head, reduce)
    def head: A
    // def tail:BList[A]
    // def reduce
  }

  private object Impl {
    def apply[A](offset: Int, block: Array[A], tail: BList[A]): Impl[A] =
      new Impl(offset, block, tail)
    def apply[A](offset: Int, block: Array[Any], tail: BList[A]): Impl[A] =
      new Impl(offset, block.asInstanceOf[Array[A]], tail)

  }

  // (maybe impl will be covariant or not)
  private case class Impl[A](offset: Int, block: Array[A], tail: BList[A]) extends NonEmpty[A] {
    def uncons = {
      val nextOffset = offset + 1
      val next = if (nextOffset == block.length) tail else Impl(nextOffset, block, tail)
      Some((block(offset).asInstanceOf[A], next))
    }
    def prepend[B >: A](a: B): BList[B] = {

      if (offset > 0) {
        // copy the right side
        val ary = block.clone().asInstanceOf[Array[B]] // replaced with copyof method to prevent having to zero out the memory first
        val nextOffset = offset - 1
        ary(nextOffset) = a 
        Impl(nextOffset, ary, tail)
      } else {
        val ary = block.clone().asInstanceOf[Array[B]]
        val offset = BlockSize - 1
        ary(offset) = a
        Impl(offset, ary, this)
      }
    }
    def head: A = {
      block(offset)
    }
    def headOption: Option[A] = {
      Some(block(offset))
    }
    def tailOption: Option[BList[A]] = {
      if (offset < BlockSize - 1) {
        Some(Impl(offset + 1, block, tail))
      } else {
        Some(tail)
      }
    }
    def get(idx: Long): Option[A] = {
      if (idx < 0) { None }
      else {
        @tailrec
        def go(idx: Long, l: BList[A]): Option[A] = {
          l match {
            case Empty                     => None
            case Impl(offset, block, tail) =>
              if (idx < BlockSize - offset) {
                Some(block(offset + idx.toInt).asInstanceOf[A])
              } else {
                go(idx - (BlockSize - offset), tail)
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
          case Empty                     => throw new NoSuchElementException("invalid index")
          case Impl(offset, block, tail) =>
            if (idx < BlockSize - offset) {
              block(offset + idx.toInt).asInstanceOf[A]
            } else {
              go(idx - (BlockSize - offset), tail)
            }
        }
      }
      go(idx, this)
    }
    def lastOption: Option[A] = {
      @tailrec
      def go(l: BList[A]): Option[A] = {
        l.asInstanceOf[Impl[A]] match {
          case Impl(_, block, tail) =>
            tail match {
              case Empty         => Some(block(BlockSize - 1).asInstanceOf[A])
              case Impl(_, _, _) => go(tail)
            }
        }
      }
      go(this)
    }
    def size: Long = {
      @tailrec
      def loop(l: BList[A], acc: Long): Long = {
        l match {
          case Empty                 => acc
          case Impl(offset, _, tail) => loop(tail, acc + (BlockSize - offset))
        }
      }
      loop(this, 0L)
    }
    def map[B](fn: A => B): BList[B] = { 
      val ary = block.clone().asInstanceOf[Array[B]]
      var i = 0
      while (i < ary.length) {
        ary(i) = fn(block(i))
        i += 1
      }
      Impl(offset, ary, tail.map(fn))
    }
    def foldLeft[B](acc: B)(fn: (B, A) => B): B = {
      @tailrec
      def loop(acc: B, l: BList[A]): B =
        l match {
          case Empty                     => acc
          case Impl(offset, block, tail) =>
            var newacc = acc
            var i = offset
            while (i < block.length) {
              newacc = fn(newacc, block(i).asInstanceOf[A])
              i += 1
            }
            loop(newacc, tail)
        }
      loop(acc, this)
    }
    def drop(n: Long): BList[A] = {
      @tailrec
      def go(n: Long, l: BList[A]): BList[A] = {
        l match {
          case Empty =>
            Empty
          case Impl(offset, block, tail) =>
            if (n >= BlockSize - offset) {
              go(n - (BlockSize - offset), tail)
            } else {
              val m: Int = math.max(n.toInt, 0) // drop < 0 is the same as drop 0
              val ary = block
                .clone() // clone and new offset should do the trick because api doesnt expose the arrays so you can only access after offset
              Impl(offset + m, ary, tail)
            }
        }
      }
      go(n, this)
    }
    def combineK[B >: A](l2: BList[B]): BList[B] = 
  {
    // TODO add special cases for if the block is a certain amount empty to shuffle things over
    // maybe use benchmarking to find out optimal number here??

    // copy l1/this and have it's tail be replaced with l2
    //@tailrec   !!! not tail rec rn !!!
    def go(l: BList[A]): BList[B] = {
      l.asInstanceOf[Impl[A]] match {
        case Impl(offset, block, tail) =>
          tail match {
            case Empty         => l2 
            case Impl(_, _, _) => 
              val ary =  block.clone().asInstanceOf[Array[B]]
              Impl(offset, ary, go(tail))
          }
      }
    }

    // for now only check if both are empty
    (this, l2) match {
      case (_, Empty) => this
      case (_,_) => go(this)

    }
  }

  def toList: List[A] = {
    // TODO OBVIOUSLY NOT NIL
    Nil

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
          case Impl(offset,block, tail)=> fnblock(block,offset, go(tail, acc, fnblock))
        }
      }
      go(this, acc, fnblock)
    }
     */

  }

  def empty[A]: BList[A] = Empty
  def unapply[A](l: BList[A]): Option[(A, BList[A])] = l.uncons
}
