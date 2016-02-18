package dogs

import Predef._
import scala.{inline,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.{tailrec}
import dogs.bedazzle.birds._

/**
 * Immutable, singly-linked list implementation.
 *
 * This code is very similar to scala.List, with a few key differences:
 *
 * 1. It does not expose any "unsafe" methods.
 * 2. It is invariant, whereas scala.List is covariant.
 * 3. It uses subtyping to differentiate non-emptiness.
 *
 * The types defined here are as follows:
 *
 *  - List[A] represents a list of zero-or-more elements.
 *  - Nel[A] represents a list of one-or-more elements.
 *  - El[A]  represents an empty list (exactly zero elements).
 *
 * (Every List[A] is either a Nel[A] or an El[A].)
 *
 * While it does not provide every single Scala collection method, it
 * provides a decent number of them. 
 */
sealed abstract class List[A] {
  import Option._

  def isEmpty: Boolean

  final def toNel: Option[Nel[A]] =
    this match {
      case nel: Nel[_] => Some(nel)
      case El() => None()
    }

  /**
   * Prepend the given value to this List
   * O(1)
   */
  final def ::(a: A): Nel[A] =
    new Nel(a, this)

  /**
   * A left-associated fold of the List, which accumuates a B value by
   * passing each element of the List to the given accumulating
   * function.
   * O(n)
   */
  @tailrec final def foldLeft[B](b: B)(f: (B, A) => B): B = {
    this match {
      case Nel(h, t) => t.foldLeft(f(b, h))(f)
      case _ => b
    }
  }

  /**
   * A right-associative fold on the list which evaluates the tail of
   * the list lazily, allowing this computation to terminate before
   * evailuating all of the elements on the list
   * O(n)
   */
  final def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = this match {
    case Nel(head, tail) => 
      f(head, Eval.defer(tail.foldr(b)(f)))
    case _ => b
  }

  /**
   * The catamorphism on List, this function is a right associative fold
   */
  def foldRight[B](b: B)(f: (A, B) => B): B =
    this.reverse.foldLeft(b)((b,a) => f(a,b))

  /**
   * Append a list to this list. 
   * O(n) on the size of this list
   */
  def :::(as: List[A]): List[A] = this match {
    case El() => as
    case x: Nel[A] => foldRight(as)(_ :: _)
  }

  /**
   * Append a list to this list. 
   * O(n) on the size of this list
   */
  def ++(as: List[A]): List[A] = this match {
    case El() => as
    case x: Nel[A] => foldRight(as)(_ :: _)
  }

  /**
   * Apply a function to each element of this list, producing a new
   * list with the results.  
   * O(n)
   */
  def map[B](f: A => B): List[B] = {
    val lb = new ListBuilder[B]

    @tailrec def go(l: List[A]): Unit = {
      l match {
        case El() =>
        case h Nel t =>
          lb += f(h)
          go(t)
      }
    }

    go(this)
    lb.run
  }

  /**
   * Apply a function returning a List to each element of this List,
   * return a List which is the concatenation of all the resulting
   * Lists.
   * O(n)
   */
  final def flatMap[B](f: A => List[B]): List[B] = {
    val lb = new ListBuilder[B] 

    @tailrec def go(l: List[A]): Unit = {
      l match {
        case El() =>
        case h Nel t =>
          f(h).foldLeft(())((_,b) => lb += b)
          go(t)
      }
    }

    go(this)
    lb.run
  }

  /**
   * Apply a function extracting a B from every sublist, accumuating
   * all the Bs into a List
   * O(n)
   */
  def coflatMap[B](f: List[A] => B): List[B] = {
    @tailrec def loop(cur: List[A], acc: List[B]): List[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(this, List.empty)
  }


  /**
   * Construct a new List containing only elements of this List which
   * pass the given predicate
   * O(n)
   */
  final def filter(pred: A => Boolean): List[A] = {
    val lb = new ListBuilder[A]
    def go(l: List[A]): Unit = l match {
      case El() =>
      case h Nel t =>
        if(pred(h)) lb += h
        go(t)
    }

    go(this)
    lb.run
  }

  /**
   * Return the first element in the List matching the given
   * predicate, if one is found at all.
   * O(n)
   */
  @tailrec final def find(pred: A => Boolean): Option[A] =
    this match {
      case El() => None()
      case Nel(h, t) => if (pred(h)) Some(h) else t.find(pred)
    }

  /**
   * Returns true of any element in the List matches the given
   * predicate.
   * O(n)
   */
  final def exists(pred: A => Boolean): Boolean =
    find(pred).isSome


  /**
   * Returns true of all elements in the List match the given
   * predicate.
   * O(n)
   */
  final def forall(p: A => Boolean): Boolean =
    find(a => !p(a)).isNone

  /**
   * Returns true if the given value is present in the List.
   * O(n)
   */
  final def contains(a: A)(implicit ev: Eq[A]): Boolean =
    find(ev.eqv(_,a)).isSome

  /**
   * Return a List which contains all of the same elements as this
   * List, but in the opposite order
   * O(n)
   */
  def reverse: List[A] = foldLeft[List[A]](List.empty)((l,a) => a :: l)

  /**
   * Returns a List containing the first n elements of this List, if n
   * < the length of this list, the result will be a copy of this
   * list.
   * O(num)
   */
  def take(num: Int): List[A] = {
    val lb: ListBuilder[A] = new ListBuilder[A]

    def go(l: List[A], n: Int): Unit = if(n > 0) {
      l match {
        case El() =>
        case h Nel t =>
          lb += h
          go(t, n - 1)
      }
    }

    go(this, num)
    lb.run
  }

  /**
   * Returns a List containing the first n elements of this List, 
   * if n * < the length of this list, the result will be a copy 
   * of this list.
   * O(num)
   */
  @tailrec final def drop(num: Int): List[A] =
    if (num <= 0) this else this match {
      case Nel(h, t) => t.drop(num - 1)
      case x => x
    }

  override def toString: String = {
    def loop(sb: StringBuilder, h: A, t: List[A]): String =
      t match {
        case El() =>
          sb.append(h).append(")").toString
        case Nel(th, tt) =>
          loop(sb.append(h).append(", "), th, tt)
      }
    this match {
      case El() => "El()"
      case Nel(h, t) => loop(new StringBuilder("List("), h, t)
    }
  }

  def toScalaList: scala.List[A] =
    foldRight[scala.List[A]](scala.Nil)(_ :: _)
}

final case class Nel[A](head: A, private[dogs] var _tail: List[A]) extends List[A] {
  def tail = _tail

  override final def isEmpty: Boolean = false

  final def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  override final def :::(as: List[A]): Nel[A] =
    as.foldr(Now(this))((a, lbs) => lbs.map(a :: _)).value

  final def :::(as: Nel[A]): Nel[A] =
    as.foldr(Now(this))((a, lbs) => lbs.map(a :: _)).value

  override final def map[B](f: A => B): Nel[B] = {
    val h = f(head)
    val t = tail.foldr(Now(List.empty[B])) { (a, lbs) =>
      lbs.map(f(a) :: _)
    }.value
    Nel(h, t)
  }

  final def coflatMap[B](f: Nel[A] => B): Nel[B] = {
    @tailrec def loop(cur: List[A], acc: Nel[B]): Nel[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(tail, List(f(this)))
  }

  override final def reverse: Nel[A] =
    tail.foldLeft(List(head))((lst, a) => a :: lst)

  override final def take(n: Int): List[A] = {
    @tailrec def loop(i: Int, acc: List[A], rest: List[A]): List[A] =
      if (i >= n) acc else rest match {
        case El() => acc
        case Nel(h, t) => loop(i + 1, h :: acc, t)
      }
    loop(0, List.empty, reverse)
  }
}

final case object El extends List[Nothing] {
  final override def isEmpty: Boolean = true
  @inline final def apply[A] = this.asInstanceOf[List[A]]
  @inline final def unapply[A](l: List[A]) = l.isEmpty
}

object List {
  final def empty[A]: List[A] =
    El.asInstanceOf[List[A]]

  final def apply[A](a: A): Nel[A] =
    Nel(a, List.empty)

  final def apply[A](a1: A, a2: A, as: A*): Nel[A] =
    a1 :: a2 :: fromIterable(as)

  final def fromIterable[A](as: Iterable[A]): List[A] =
    as.foldLeft(List.empty[A])((lst, a) => a :: lst).reverse

  def fill[A](n: Int)(a: => A): List[A] = {
    @tailrec
    def go(n: Int, a: => A, l: List[A]): List[A] =
      if(n > 0) go(n-1, a, a :: l) else l

    go(n, a, List.empty[A])
  }
}

final private[dogs] class ListBuilder[A] {
  import List.empty
  var run: List[A] = List.empty
  var end: Nel[A] = _

  def +=(a: A): Unit = {
    run match {
      case El() =>
        end = Nel(a, empty[A])
        run = end
      case _ =>
        val newEnd = Nel(a, empty[A])
        end._tail = newEnd
        end = newEnd
    }
  }

  def nonEmpty: Boolean = run != El
  def isEmpty: Boolean = run == El
  def toList:List[A] = run
}
