/**
 * Created by Nicolas A Perez (@anicolaspp) on 2/4/16.
 */

package dogs

import dogs.Order._
import dogs.Predef._

/**
  * Discrete Interval Encoding Tree (Diet).
 * It stores subsets of types having a total order, a predecessor and a successor function described by Enum[A]
 *
 * Diet is a binary search tree where each node contains a range of values and the set of all nodes is a set of
 * disjoint sets.
 *
 * In the best case, when there are no "holes" in the stored set, the interval representation consists of just one
 * single interval (node) and finding, inserting and deleting operations are O(1). In the worse case, where there
 * are not two adjacent elements in the set, the representation is equivalent to a binary search tree.
 */
sealed abstract class Diet[A] {
  import Diet._
  import dogs.Range.EmptyRange

  val isEmpty: Boolean

  /**
    * verify x is a value is in the tree
    */
  def contains(x: A)(implicit discrete: Enum[A]): Boolean = this match {
    case EmptyDiet()            =>  false
    case DietNode(a, b, l, r)   => (discrete.compare(x, a), discrete.compare(x, b)) match {
        case (EQ, _)  =>  true
        case (_, EQ)  =>  true
        case (GT, LT) =>  true
        case (LT, _)  =>  l.contains(x)
        case (_, GT)  =>  r.contains(x)
      }
  }

  /**
    * return a list of all disjoint sets in the tree where each set is represented by ARange
    */
  def disjointSets: List[Range[A]] = this match {
    case EmptyDiet()          =>  El()
    case DietNode(x, y, l, r) =>  l.disjointSets ::: (Range(x, y) :: r.disjointSets)
  }

  /**
    * convert tree in a sorted list from all disjoint sets in the tree
    */
  def toList()(implicit discrete: Enum[A]): List[A] =
    disjointSets.flatMap(lst => lst.generate())

  /**
   * add new value range [x, y] to de tree.
   *
   * Cases:
   *  Trivial:
   *    (1) [x, y] is a subrange of an existing range
   *    (2) [x, y] extends diet to the left st x < min(diet) => left(diet) is not needed
   *    (3) same as (2) but to the right
   *  Non-Trivial:
   *    (4) [x, y] extends diet to the left st x > min(diet) => extend diet and re-insert ranges in left(diet)
   *    (5) [x, y] does not extend diet, [x, y] has to be insert into left(diet)
   *    (6) same as (4) to the right
   *    (7) same as (5) to the right
   */
  def add(range: Range[A])(implicit discrete: Enum[A]): Diet[A] = (this, range) match {
    case (_, EmptyRange())              =>  this
    case (EmptyDiet(), r)               =>  DietNode(r.start, r.end, EmptyDiet(), EmptyDiet())
    case (d @ DietNode(x, y, l, r), rng)    => {

      val (m, n) = rng - (Range(x, y))

      if (Range(x, y).contains(rng)) {                                      //(1)
        this
      }
      else if (isBestLeft(rng, d) && discrete.adj(m.end, x)) {              //(2)
        DietNode(rng.start, y, EmptyDiet(), r).asInstanceOf[Diet[A]].add(n)
      }
      else if (isBestRight(rng, d) && discrete.adj(y, n.start)){            //(3)
        DietNode(x, rng.end, l, EmptyDiet())
      }
      else {                                                                //More complex case
        val root = {
          if (!m.isEmpty && discrete.adj(m.end, x)) {                       //(4)
            val li = DietNode(m.start, y, EmptyDiet(), EmptyDiet())
            val t = l.disjointSets.foldLeft[Diet[A]](li)((d, r) => d.add(r))

            t.asInstanceOf[DietNode[A]]
          }
          else {
            DietNode(x, y, l.add(m), r)                                     //(5)
          }
        }

        if (!n.isEmpty && discrete.adj(y, n.start)) {                       //(6)
          val ri = DietNode(root.x, n.end, EmptyDiet(), EmptyDiet())

          r.disjointSets.foldLeft[Diet[A]](ri)((d, r) => d.add(r))
        }
        else {
          DietNode(root.x, y, root.left, r.add(n))                          //(7)
        }
      }
    }
  }

  /**
    * add new value range [x, y] to de tree. If x > y then it will add range [y, x]
    */
  def add(x: A, y: A)(implicit discrete: Enum[A]): Diet[A] = {
    if (discrete.compare(x, y) == EQ) {
      add(x)
    }
    else if (discrete.compare(x, y) == LT) {
      add(Range(x, y))
    }
    else {
      add(Range(y, x))
    }
  }

  /**
    * add new value to de tree
    */
  def add(value: A)(implicit discrete: Enum[A]): Diet[A] = this match {
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

  /**
    * remove x from the tree
    */
  def remove(x: A)(implicit discrete: Enum[A]): Diet[A] = this match {
    case EmptyDiet()  =>  this
    case DietNode(a, b, l, r) =>  {
      if (discrete.compare(x, a) == LT) {
        DietNode(a, b, l.remove(x), r)
      }
      else if (discrete.compare(x, b) == GT) {
        DietNode(a, b, l, r.remove(x))
      }
      else if (discrete.compare(x, a) == EQ) {
        if (discrete.compare(a, b) == EQ)
          merge(l, r)
        else
          DietNode(discrete.succ(a), b, l, r)
      }
      else if (discrete.compare(x, b) == EQ) {
        DietNode(a, discrete.pred(b), l, r)
      }
      else {
        DietNode(a, discrete.pred(x), l, DietNode(discrete.succ(x), b, EmptyDiet(), r))
      }
    }
  }

  /**
    * alias for add
    */
  def +(value: A)(implicit discrete: Enum[A]): Diet[A] = add(value)

  /**
   * alias for add
   */
  def +(range: Range[A])(implicit discrete: Enum[A]): Diet[A] = add(range)

  /**
    * alias for remove
    */
  def -(value: A)(implicit discrete: Enum[A]): Diet[A] = remove(value)

  /**
    * min value in the tree
    */
  def min: Option[A] = this match {
    case EmptyDiet()  =>  None()
    case DietNode(x, _, EmptyDiet(), _) => Some(x)
    case DietNode(_, _, l, _) => l.min
  }

  /**
    * max value in the tree
    */
  def max: Option[A] = this match {
    case EmptyDiet()  => None()
    case DietNode(_, y, _, EmptyDiet()) => Some(y)
    case DietNode(_, _, _, r) => r.max
  }

  def map[B](f: A => B)(implicit discrete: Enum[B]): Diet[B] = this match {
    case EmptyDiet()          =>  Diet.empty[B]
    case DietNode(a, b, l, r) =>  {
      val (lp, rp) = (l.map(f), r.map(f))

      val n = lp + f(a) + f(b)

      merge(n, rp)
    }
  }

  def foldRight[B](s: B)(f: (B, A) => B)(implicit discrete: Enum[A]): B = this match {
    case EmptyDiet()          =>  s
    case DietNode(a, b, l, r) =>  l.foldRight(Range(a, b).reverse().foldLeft(r.foldRight(s)(f))(f))(f)
  }

  def foldLeft[B](s: B)(f: (B, A) => B)(implicit discrete: Enum[A]): B = this match {
    case EmptyDiet()          =>  s
    case DietNode(a, b, l, r) =>  r.foldLeft(Range(a, b).generate().foldLeft(l.foldLeft[B](s)(f))(f))(f)
  }
}

object Diet {
  def apply[A]: Diet[A] = empty

  def empty[A]: Diet[A] = EmptyDiet()

  private [dogs] case class DietNode[A](x: A, y: A, left: Diet[A], right: Diet[A]) extends Diet[A] {
    override val isEmpty: Boolean = false
  }

  private [dogs] case object EmptyDiet extends Diet[Nothing] {
    def unapply[A](diet: Diet[A]): Boolean = diet.isEmpty

    def apply[A](): Diet[A] = this.asInstanceOf[Diet[A]]

    override val isEmpty: Boolean = true
  }

  /**
    * merge to Diets
    * It does not merge overlapping Diet(s). Here is more work to do
    */
  private [dogs] def merge[A](l: Diet[A], r: Diet[A]): Diet[A] = (l, r) match {
    case (l, EmptyDiet())   =>  l
    case (EmptyDiet(), r)   =>  r
    case (l, r)             =>  {
      val (lp, i) = splitMax(l)

      DietNode(i._1, i._2, lp, r)
    }
  }

  private [dogs] def splitMin[A](n: Diet[A]): (Diet[A], (A, A)) = n match {
    case DietNode(x, y, EmptyDiet(), r)   => (r, (x, y))
    case DietNode(x, y, l, r)             => {
      val (d, i) = splitMin(l)

      (DietNode(x, y, d, r), i)
    }
  }

  private [dogs] def splitMax[A](n: Diet[A]): (Diet[A], (A, A)) = n match {
    case DietNode(x, y, l, EmptyDiet())   =>  (l, (x, y))
    case DietNode(x, y, l, r)             =>  {
      val (d, i) = splitMax(r)

      (DietNode(x, y,l, d), i)
    }
  }

  private [dogs] def joinLeft[A](node: DietNode[A])(implicit discrete: Enum[A]): Diet[A] = node match {
    case DietNode(_, _, EmptyDiet(), _)   =>  node
    case DietNode(x, y, l, r)             =>  {
      val (lp, (li, lj)) = splitMax(l)

      if (discrete.adj(lj, x))
        DietNode(li, y, lp, r)
      else
        DietNode(x, y, l, r)
    }
  }

  private [dogs] def joinRight[A](node: DietNode[A])(implicit  discrete: Enum[A]): Diet[A] = node match {
    case DietNode(_, _, _, EmptyDiet())   =>  node
    case DietNode(x, y, l, r)             =>  {
      val (rp, (ri, rj)) = splitMin(r)

      if (discrete.adj(y, ri))
        DietNode(x, rj, l, rp)
      else
        DietNode(x, y, l, r)
    }
  }

  private [dogs] def isBestLeft[A](r: Range[A], dietNode: DietNode[A])(implicit discrete: Enum[A]): Boolean =
    dietNode.min match {
      case None() => false
      case Some(a) => {
        if (discrete.lt(r.start, a)) {
          true
        }
        else {
          false
        }
      }
    }

  private [dogs] def isBestRight[A](r: Range[A], dietNode: DietNode[A])(implicit discrete: Enum[A]): Boolean =
    dietNode.max match {
      case None()       =>  false
      case Some(a)      =>  {
        if (discrete.gt(r.end, a)) {
          true
        }
        else {
          false
        }
      }
    }
}

/**
 * Represent discrete operations that can be performed on A
 */
trait Enum[A] extends Order[A] {

  /**
   * return the successor of x
   */
  def succ(x: A): A

  /**
   * return the predecessor of
   */
  def pred(x: A): A

  /**
   * verify if x and y are consecutive
   */
  def adj(x: A, y: A): Boolean = succ(x) == y
}

/**
  * Represent discrete operations that can be performed on BigInt
  */
sealed class BigIntEnum extends Enum[BigInt] {
  override def succ(x: BigInt): BigInt = x + 1

  override def pred(x: BigInt): BigInt = x - 1

  override def apply(l: BigInt, r: BigInt): Ordering = if (l < r) LT else if (l > r) GT else EQ
}








