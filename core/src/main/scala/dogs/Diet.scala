/**
 * Created by Nicolas A Perez (@anicolaspp) on 2/4/16.
 */


package dogs

import Predef._
import dogs.Diet.{EmptyDiet, DietNode}
import dogs.Order._

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq




trait Discrete[A] extends Order[A]{
  def succ(x: A): A
  def pred(x: A): A
  def adj(x: A, y: A): Boolean = succ(x) == y
}

trait ARange[A] {
  def apply(start: A, end: A): ARange[A]
  def generate()(implicit discrete: Discrete[A]): List[A]
}

sealed class DRange[A](val start: A, val end: A) extends ARange[A]{

  def generate()(implicit discrete: Discrete[A]): List[A] = {
    @tailrec def genRange(a: A, b: A, xs: List[A])(implicit discrete: Discrete[A]): List[A] = {
      if (discrete.compare(a, b) == EQ) {
        xs ::: Nel(a, El())
      } else if (discrete.adj(a, b)) {
        xs ::: Nel(a, Nel(b, El()))
      }
      else {
        genRange(discrete.succ(a), b, xs ::: (Nel(a, El())))
      }
    }

    genRange(start, end, El())
  }

  override def apply(start: A, end: A): DRange[A] = DRange.apply(start, end)
}

object DRange {
  def apply[A](x: A, y: A) = new DRange[A](x, y)
}

sealed abstract class Diet[A] {
  def contains(x: A)(implicit discrete: Discrete[A]): Boolean = this match {
    case EmptyDiet()          => false
    case DietNode(a,b, l, r)  =>  (discrete.compare(x, a), discrete.compare(x, b)) match {
      case (EQ, _)  =>  true
      case (_, EQ)  =>  true
      case (GT, LT) =>  true
      case (LT, _)  =>  l.contains(x)
      case (_, GT)  =>  r.contains(x)
    }
  }

  val isEmpty: Boolean

  def disjointSets(): List[DRange[A]] = this match {
    case EmptyDiet()          =>  El()
    case DietNode(x, y, l, r) =>  l.disjointSets() ::: (DRange(x, y) :: r.disjointSets())
  }

  def toList()(implicit discrete: Discrete[A]): List[A] =
    disjointSets().flatMap(lst => lst.generate())

  def add(x: A, y: A)(implicit discrete: Discrete[A]): Diet[A] = {
    if (discrete.compare(x, y) == EQ) {
      add(x)
    }
    else {
      add(x).add(discrete.succ(x), y)
    }
  }

  def add(value: A)(implicit discrete: Discrete[A]): Diet[A] = this match {
    case EmptyDiet()                  =>  DietNode[A](value, value, EmptyDiet(), EmptyDiet())
    case d @ DietNode(x, y, l, r)     => {
      if (discrete.compare(value, x) == LT) {
        if (discrete.adj(value, x)) {
          joinLeft(DietNode(value, y, l, r))(discrete)
        } else {
          DietNode(x, y, l.add(value), r)
        }
      }
      else if (discrete.compare(value, y) == GT){
          if (discrete.adj(y, value))
            joinRight(DietNode(x, value, l, r))
          else
            DietNode(x, y, l, r.add(value))
      }
      else d
    }
  }

  def +(value: A)(implicit discrete: Discrete[A]): Diet[A] = add(value)

  def min: Option[A] = this match {
    case EmptyDiet()  =>  None()
    case DietNode(x, _, EmptyDiet(), _) => Some(x)
    case DietNode(_, _, l, _) => l.min
  }

  def max: Option[A] = this match {
    case EmptyDiet()  => None()
    case DietNode(_, y, _, EmptyDiet()) => Some(y)
    case DietNode(_, _, _, r) => r.max
  }

  private [dogs] def splitMin(n: Diet[A]): (Diet[A], (A, A)) = n match {
    case DietNode(x, y, EmptyDiet(), r)   => (r, (x, y))
    case DietNode(x, y, l, r)             => {
      val (d, i) = splitMin(l)

      (DietNode(x, y, d, r), i)
    }
  }

  private [dogs] def splitMax(n: Diet[A]): (Diet[A], (A, A)) = n match {
    case DietNode(x, y, l, EmptyDiet())   =>  (l, (x, y))
    case DietNode(x, y, l, r)             =>  {
      val (d, i) = splitMax(r)

      (DietNode(x, y,l, d), i)
    }
  }

  private [dogs] def joinLeft(node: DietNode[A])(implicit discrete: Discrete[A]): Diet[A] = node match {
    case DietNode(_, _, EmptyDiet(), _)   =>  node
    case DietNode(x, y, l, r)             =>  {
      val (lp, (li, lj)) = splitMax(l)

      if (discrete.adj(lj, x))
        DietNode(li, y, lp, r)
      else
        DietNode(x, y, l, r)
    }
  }

  private [dogs] def joinRight(node: DietNode[A])(implicit  discrete: Discrete[A]): Diet[A] = node match {
    case DietNode(_, _, _, EmptyDiet())   =>  node
    case DietNode(x, y, l, r)             =>  {
      val (rp, (ri, rj)) = splitMin(r)

      if (discrete.adj(y, ri))
        DietNode(x, rj, l, rp)
      else
        DietNode(x, y, l, r)
    }
  }
}

object Diet {
  def apply[A](): Diet[A] = empty()

  def empty[A](): Diet[A] = EmptyDiet()

  private [dogs] case class DietNode[A](x: A, y: A, left: Diet[A], right: Diet[A]) extends Diet[A] {
    override val isEmpty: Boolean = false
  }

  private [dogs] case object EmptyDiet extends Diet[Nothing] {
    def unapply[A](diet: Diet[A]): Boolean = diet.isEmpty

    def apply[A](): Diet[A] = this.asInstanceOf[Diet[A]]

    override val isEmpty: Boolean = true
  }
}




