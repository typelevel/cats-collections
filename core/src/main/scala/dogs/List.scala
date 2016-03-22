package dogs

import Predef._
import scala.{inline,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.{tailrec}
import dogs.syntax.birds._
import cats._

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
  final def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = this match {
    case Nel(head, tail) => 
      f(head, Eval.defer(tail.foldRight(b)(f)))
    case _ => b
  }


  /**
   * Execute the side-effecting function on each memeber of the list, in order
   */
  def foreach(f: A => Unit): Unit = this match {
    case h Nel t =>
      f(h)
      t foreach f
    case _ => ()
  }

  /**
   * Return the head of the list, if one exists
   */
  final def headOption: Option[A] = this match {
    case h Nel _ => Option.some(h)
    case _ => Option.none
  }

  /**
   * Return the tail of the list, if one exists
   */
  final def tailOption: Option[List[A]] = this match {
    case _ Nel t => Option.some(t)
    case _ => Option.none
  }


  /**
   * Append a list to this list. 
   * O(n) on the size of this list
   */
  def :::(as: List[A]): List[A] = this match {
    case El() => as
    case x: Nel[A] => foldRight(Eval.now(as))((a,las) => las.map(_.::(a))).value
  }

  /**
   * Append a list to this list. 
   * O(n) on the size of this list
   */
  def ++(as: List[A]): List[A] = this match {
    case El() => as
    case x: Nel[A] => foldRight(Eval.now(as))((a,las) => las.map(_.::(a))).value
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
          f(h).foldLeft(()){(_,b) => val _ = lb += b}
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
    foldRight[scala.List[A]](Eval.now(scala.Nil))((a,las) => las.map(_.::(a))).value
}

final case class Nel[A](head: A, private[dogs] var _tail: List[A]) extends List[A] {
  def tail = _tail

  override final def isEmpty: Boolean = false

  final def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  override final def :::(as: List[A]): Nel[A] =
    as.foldRight(Eval.now(this))((a, lbs) => lbs.map(a :: _)).value

  final def :::(as: Nel[A]): Nel[A] =
    as.foldRight(Eval.now(this))((a, lbs) => lbs.map(a :: _)).value

  override final def map[B](f: A => B): Nel[B] = {
    val h = f(head)
    val t = tail.foldRight(Eval.now(List.empty[B])) { (a, lbs) =>
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
    val lb = new ListBuilder[A]
    @tailrec def loop(i: Int, l: List[A]): Unit =
      if(i > 0)
        l match {
          case h Nel t =>
            lb += h
            loop(i - 1, t)
          case _ => ()
        }

    loop(n, this)
    lb.run
  }
}

final case object El extends List[Nothing] {
  final override def isEmpty: Boolean = true
  @inline final def apply[A] = this.asInstanceOf[List[A]]
  @inline final def unapply[A](l: List[A]) = l.isEmpty
}

object List extends ListInstances {
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

sealed trait ListInstances {
  implicit def listCmp[A](implicit A: Order[A]): Order[List[A]] = new Order[List[A]] {
    override def compare(a: List[A], b: List[A]): Int = (a,b) match {
      case (El(), El()) =>  0
      case (El(), _)    =>  -1
      case (_, El())    =>  1

      case (Nel(ha, ta), Nel(hb, tb)) =>
        val cmp = A.compare(ha, hb)
        if(cmp == 0)
          this.compare(ta, tb)
        else
          cmp
    }
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override val empty: List[A] = List.empty
    override def combine(l: List[A], r: List[A]) = l ::: r
  }

  implicit val listInstance: Traverse[List] with MonadCombine[List] with CoflatMap[List] =
    new Traverse[List] with MonadCombine[List] with CoflatMap[List] {

      override def empty[A]: List[A] = List.empty

      override def combineK[A](l: List[A], r: List[A]): List[A] = l ++ r

      override def pure[A](a: A): List[A] = a :: List.empty

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      override def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = fa coflatMap f

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
        import scala.collection.immutable.Vector
        val gba = G.pure(Vector.empty[B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ :+ _))
        G.map(gbb)(_.foldRight[List[B]](List.empty)(_ :: _))
      }

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      /** not until we have streaming in dogs
      override def toStreaming[A](fa: List[A]): Streaming[A] =
        Streaming.fromList(fa)
       */
    }

  implicit def listShow[A](implicit A: Show[A]): Show[List[A]] =
    new Show[List[A]] {
      def show(fa: List[A]): String = fa.map(A.show).toString
    }

  implicit def listEq[A](implicit A: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
    override def eqv(a: List[A], b: List[A]): Boolean = (a,b) match {
      case (El(), El()) => true
      case (El(), _) => false
      case (_, El()) => false
      case (Nel(ha,ta), Nel(hb,tb)) =>
        if(A.eqv(ha, hb)) eqv(ta,tb) else false
    }
  }
}

object Nel extends NelInstances

trait NelInstances {
  implicit def nelCmp[A](implicit A: Order[A]): Order[Nel[A]] =
    new Order[Nel[A]] {
      override def compare(a: Nel[A], b: Nel[A]): Int = {
        val cmp = A.compare(a.head, b.head)
        if(cmp == 0) {
          return List.listCmp[A].compare(a.tail, b.tail)
        } else cmp
      }
    }
}



final private[dogs] class ListBuilder[A] {
  import List.empty
  var run: List[A] = List.empty
  var end: Nel[A] = _

  def +=(a: A): ListBuilder[A] = {
    run match {
      case El() =>
        end = Nel(a, empty[A])
        run = end
        this
      case _ =>
        val newEnd = Nel(a, empty[A])
        end._tail = newEnd
        end = newEnd
        this
    }
  }
}
