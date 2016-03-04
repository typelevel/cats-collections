package dogs

import cats.Show
import dogs.Predef._
import algebra.Order


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

  import Range.EmptyRange

  val isEmpty: Boolean

  /**
   * Returns true if x is a value is in the tree.
   */
  def contains(v: A)(implicit order: Order[A]): Boolean = this match {
    case EmptyDiet()            =>  false
    case DietNode(a, b, l, r)   => (order.compare(v, a), order.compare(v, b)) match {
        case (0, _)  =>  true
        case (_, 0)  =>  true
        case (x, y) if x > 0 && y < 0 =>  true
        case (x, _) if x < 0 =>  l.contains(v)
        case (_, y) if y > 0 =>  r.contains(v)
      }
  }

  def containsRange(range: Range[A])(implicit order: Order[A]): Boolean = this match {
    case EmptyDiet()            =>  false
    case DietNode(x, y, l, r)   => {
      if (Range(x, y).contains(range)){
        true
      }
      else if (order.lt(range.start, x)) {
        l.containsRange(range)
      } else {
        r.containsRange(range)
      }
    }
  }

  /**
   * Returns a list of all disjoint sets in the tree where each set is
   * represented by ARange.
   */
  def intervals: List[Range[A]] = this match {
    case EmptyDiet()          =>  List.empty
    case DietNode(x, y, l, r) =>  l.intervals ::: (Range(x, y) :: r.intervals)
  }

  /**
   * Convert tree in a sorted list from all disjoint sets in the tree.
   */
  def toList()(implicit discrete: Enum[A], order: Order[A]): List[A] =
    intervals.flatMap(lst => lst.generate)

  /**
   * Add new value range [x, y] to the Diet.
   */
  def addRange(range: Range[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = (this, range) match {
   // Cases:
   //  Trivial:
   //    (1) [x, y] is a subrange of an existing range
   //    (2) [x, y] extends diet to the left st x < min(diet) => left(diet) is not needed
   //    (3) same as (2) but to the right
   //  Non-Trivial:
   //    (4) [x, y] extends diet to the left st x > min(diet) => extend diet and re-insert ranges in left(diet)
   //    (5) [x, y] does not extend diet, [x, y] has to be insert into left(diet)
   //    (6) same as (4) to the right
   //    (7) same as (5) to the right
   // 
    case (_, EmptyRange())                =>  this
    case (EmptyDiet(), r)                 =>  DietNode(r.start, r.end, EmptyDiet(), EmptyDiet())
    case (d @ DietNode(x, y, l, r), rng)  => {

      val (m, n) = rng - (Range(x, y))

      if (Range(x, y).contains(rng)) {                                      //(1)
        this
      } else if (isBestLeft(rng, d) && discrete.adj(m.end, x)) {            //(2)
        DietNode(rng.start, y, EmptyDiet(), r) + n
      } else if (isBestRight(rng, d) && discrete.adj(y, n.start)){          //(3)
        DietNode(x, rng.end, l, EmptyDiet()) + m
      } else {                                                              //More complex cases
        val root = {
          if (!m.isEmpty && discrete.adj(m.end, x)) {                       //(4)
            val li = DietNode(m.start, y, EmptyDiet(), EmptyDiet())
            val t = l.intervals.foldLeft[Diet[A]](li)((d, r) => d.addRange(r))

            t.asInstanceOf[DietNode[A]]
          }
          else {
            DietNode(x, y, l.addRange(m), r)                                //(5)
          }
        }

        if (!n.isEmpty && discrete.adj(y, n.start)) {                       //(6)
          val ri = DietNode(root.x, n.end, root.left, EmptyDiet())

          r.intervals.foldLeft[Diet[A]](ri)((d, r) => d.addRange(r))
        }
        else {
          DietNode(root.x, y, root.left, r.addRange(n))                     //(7)
        }
      }
    }
  }

  /**
   * Adds new value to the tree.
   */
  def add(value: A)(implicit discrete: Enum[A], order: Order[A]): Diet[A] = this match {
    case EmptyDiet()                  =>  DietNode[A](value, value, EmptyDiet(), EmptyDiet())
    case d @ DietNode(x, y, l, r)     => {
      if (order.compare(value, x) < 0) {
        if (discrete.adj(value, x)) {
          joinLeft(DietNode(value, y, l, r))(discrete)
        } else {
          DietNode(x, y, l.add(value), r)
        }
      }
      else if (order.compare(value, y) > 0){
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
  def remove(x: A)(implicit discrete: Enum[A], order: Order[A]): Diet[A] = this match {
    case EmptyDiet()          =>  this
    case DietNode(a, b, l, r) =>  {
      if (order.compare(x, a) < 0) {
        DietNode(a, b, l.remove(x), r)
      }
      else if (order.compare(x, b) > 0) {
        DietNode(a, b, l, r.remove(x))
      }
      else if (order.compare(x, a) == 0) {
        if (order.compare(a, b) == 0)
          merge(l, r)
        else
          DietNode(discrete.succ(a), b, l, r)
      }
      else if (order.compare(x, b) == 0) {
        DietNode(a, discrete.pred(b), l, r)
      }
      else {
        DietNode(a, discrete.pred(x), l, DietNode(discrete.succ(x), b, EmptyDiet(), r))
      }
    }
  }

  /**
    * remove a range from Diet
    */
  def removeRange(range: Range[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = (this, range) match {
    case (_, EmptyRange())            => this
    case (EmptyDiet(), _)             => this
    case (DietNode(a, b, l, r), rng)  => {
      if (order.compare(a, rng.start) == 0 &&
        order.compare(b, rng.end) == 0) {
        merge(l, r)
      } else if (Range(a, b).contains(rng)) {
        if (order.compare(a, rng.start) == 0) {
          merge(l, DietNode(discrete.succ(rng.end), b, EmptyDiet(), r))
        } else if (order.compare(b, rng.end) == 0) {
          merge(DietNode(a, discrete.pred(rng.start), l, EmptyDiet()), r)
        } else {
          merge(
            DietNode(a, discrete.pred(rng.start), l, EmptyDiet()),
            DietNode(discrete.succ(rng.end), b, EmptyDiet(), r)
          )
        }
      } else {
        (Range(a, b) - rng) match {
          case (EmptyRange(), EmptyRange()) =>  merge(l - rng, r - rng)
          case (m, EmptyRange())            =>  merge(DietNode(m.start, m.end, l, EmptyDiet()), r - rng)
          case (EmptyRange(), n)            =>  merge(l - rng, DietNode(n.start, n.end, EmptyDiet(), r))
        }
      }
    }
  }

  /**
   * alias for add
   */
  def +(value: A)(implicit discrete: Enum[A], order: Order[A]): Diet[A] = add(value)

  /**
   * alias for add
   */
  def +(range: Range[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = addRange(range)

  /**
   * alias for remove
   */
  def -(value: A)(implicit discrete: Enum[A], order: Order[A]): Diet[A] = remove(value)

  /**
   * alias for removeRange
   */
  def -(range: Range[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = removeRange(range)

  /**
   * Returns the union of this Diet with another Diet.
   */
  def ++(that: Diet[A])(implicit discrete:Enum[A], order: Order[A]): Diet[A] = that.intervals.foldLeft(this)((d, r) => d + r)

  /**
   * Returns the union of this Diet with another Diet.
   */
  def |(that: Diet[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = this ++ that

  /**
   * intersection with the given diet
   */
  def &(that: Diet[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = (this, that) match {
    case (_, EmptyDiet()) => EmptyDiet()
    case (EmptyDiet(), _) => EmptyDiet()
    case (a, b)           => (a.intervals ::: b.intervals).foldLeft(Diet.empty[A])((d, r) =>
        if (a.containsRange(r) && b.containsRange(r))
          d + r
        else
          r.generate.foldLeft(d)((s, x) => if (a.contains(x) && b.contains(x)) s + x else s)
      )
  }

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

  def map[B: Enum: Order](f: A => B): Diet[B] = this match {
    case EmptyDiet()          =>  Diet.empty[B]
    case DietNode(a, b, l, r) =>  {
      val (lp, rp) = (l.map(f), r.map(f))

      val n = lp + f(a) + f(b)

      merge(n, rp)
    }
  }

  def foldRight[B](s: B)(f: (B, A) => B)(implicit enumA: Enum[A], orderA: Order[A]): B = this match {
    case EmptyDiet()          =>  s
    case DietNode(a, b, l, r) =>  l.foldRight(Range(a, b).reverse.toList.foldLeft(r.foldRight(s)(f))(f))(f)
  }

  def foldLeft[B](s: B)(f: (B, A) => B)(implicit enumA: Enum[A], orderA: Order[A]): B = this match {
    case EmptyDiet()          =>  s
    case DietNode(a, b, l, r) => r.foldLeft(Range(a, b).generate.foldLeft(l.foldLeft[B](s)(f))(f))(f)
  }
}

object Diet {
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
   *  Merge two Diets
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

  private [dogs] def isBestLeft[A](r: Range[A], dietNode: DietNode[A])(implicit order: Order[A]): Boolean =
    dietNode.min match {
      case None() => false
      case Some(a) => order.lt(r.start, a)
    }

  private [dogs] def isBestRight[A](r: Range[A], dietNode: DietNode[A])(implicit order: Order[A]): Boolean =
    dietNode.max match {
      case None()       =>  false
      case Some(a)      =>  order.gt(r.end, a)
    }

  implicit def dietShowable[A](implicit s: Show[A]): Show[Diet[A]] = new Show[Diet[A]] {
    override def show(f: Diet[A]): Predef.String =
      if (f.isEmpty)
        "{}"
      else {
        val m = Show[Range[A]]

        f.intervals match {
          case Nel(h, t)  =>
            t.foldLeft("{" + m.show(h))((acc, r) => acc + ", " + m.show(r)) + "}"
        }
    }
  }
}
