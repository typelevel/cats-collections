package cats.collections

trait BlockedListCopy [+T]{
    def uncons[A >: T]: Option[(A, BlockedListCopy[A])]
    def prepend[A >: T](a: A): BlockedListCopy[A]
    def forEach[U](f: T => U): Unit
    def foldLeft[B](start: B)(f: (B, T) => B): B
    def isEmpty: Boolean
  }

  object BlockedListCopy {


    def apply[A](elements: A*)(BlockSize: Int): BlockedListCopy[A] = {
      elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
    }
    def apply[A](elements: List[A])(BlockSize: Int): BlockedListCopy[A] = {
      elements.foldRight(empty[A](BlockSize))((elem, acc) => acc.prepend(elem))
    }
    def empty[A](BlockSize: Int): BlockedListCopy[A] = Empty(BlockSize)



    private case class Empty(BlockSize: Int) extends BlockedListCopy[Nothing] {

      override def uncons[A >: Nothing]: Option[(A, BlockedListCopy[A])] = None

      override def prepend[A >: Nothing](a: A): BlockedListCopy[A] = {
        val arrayBlock = new Array[Any](BlockSize)
        val offset = BlockSize - 1
        arrayBlock(offset) = a
        Impl(offset, arrayBlock, this, BlockSize)
      }

      override def isEmpty: Boolean = true

      override def forEach[U](f: Nothing => U): Unit = ()

      override def foldLeft[B](start: B)(f: (B, Nothing) => B): B = start
    }

    private case class Impl[+T](offset: Int, block: Array[Any], tail: BlockedListCopy[T], BlockSize: Int) extends BlockedListCopy[T] {
      @inline
      override def uncons[A >: T]: Option[(A, BlockedListCopy[A])] = {

        val next = if (offset + 1 < BlockSize) {
          Impl(offset + 1, block, tail, BlockSize)
        } else {
          tail
        }
        Some((block(offset).asInstanceOf[A], next))
      }

      override def prepend[A >: T](a: A): BlockedListCopy[A] = {
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
