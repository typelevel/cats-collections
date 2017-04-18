package dogs

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

  val isEmpty: Boolean

  /**
   * Returns true if x is a value is in the tree.
   */
  @tailrec final
  def contains(v: A)(implicit order: Order[A]): Boolean = this match {
    case EmptyDiet() =>  false
    case DietNode(rng, l, r) =>
      if(order.lt(v, rng.start)) l.contains(v)
      else if(order.gt(v, rng.end)) r.contains(v)
      else rng.contains(v)
  }

  /**
   * Returns true if all values in the range are contained in the tree
   */
  @tailrec final
  def containsRange(range: Range[A])(implicit order: Order[A]): Boolean = this match {
    case EmptyDiet() => false
    case DietNode(rng, l, r) => {
      if (rng.contains(range)){
        true
      } else if (order.lt(range.start, rng.start)) {
        l.containsRange(range)
      } else {
        r.containsRange(range)
      }
    }
  }

  // helper method for insertRange.
  // our parent includes a Range which starts with `a`. We must return
  // a new copy of our tree which contains no ranges greater than `a`
  // and a new lower bound for our parent range (in case a was
  // adjacent to our greatest Range)
  @tailrec
  private def noMoreThan(a: A)(implicit order: Order[A], enum: Enum[A]): (Diet[A], A) =
    this match {
      case DietNode(rng,l,r) =>
        if(order.gt(a, rng.end)) (this, a)
        else if(order.gteqv(a, rng.start)) (l, rng.start)
        else l.noMoreThan(a)
      case x => (x,a)
    }

  // helper method for insertRange.
  // our parent includes a Range which ends with `a`. We must return a
  // new copy of our tree which contains no ranges less than `a` and a
  // new upper bound for our parent range (in case a was adjacent to
  // our leaast Range)
  @tailrec
  private def noLessThan(a: A)(implicit order: Order[A], enum: Enum[A]): (Diet[A], A) =
    this match {
      case DietNode(rng,l,r) =>
        if(order.lt(a, rng.start)) (this, a)
        else if(order.lteqv(a, rng.end)) (r, rng.end)
        else r.noLessThan(a)
      case x => (x,a)
    }

  // helper method for addRange which does the actual insertion
  private [dogs] def insertRange(range: Range[A])(implicit enum: Enum[A], order: Order[A]): Diet[A] =
    this match {
      case EmptyDiet() =>  DietNode(range, EmptyDiet(), EmptyDiet())

      case DietNode(rng, l, r)  => {
        val (r1,r2) = (rng + range)
          r2 match {
            case None() =>
              val (left, start) = l.noMoreThan(r1.start)
              val (right, end) = r.noLessThan(r1.end)
              DietNode[A](Range(start,end),left,right)

            case Some(r2) =>
              if(r1 == rng)
                DietNode(r1, l, r.insertRange(r2))
              else
                DietNode(r2, l.insertRange(r1), r)
            }
      }
    }

  /**
   * Add new value range [x, y] to the Diet.
   */
  def addRange(range: Range[A])(implicit enum: Enum[A], order: Order[A]): Diet[A] =
    if (order.lteqv(range.start, range.end))
      insertRange(range)
    else
      insertRange(range.reverse)

  /**
   * Adds new value to the tree.
   */
  def add(value: A)(implicit enum: Enum[A], order: Order[A]): Diet[A] = addRange(Range(value, value))

  /**
    * remove x from the tree
    */
  def remove(x: A)(implicit enum: Enum[A], order: Order[A]): Diet[A] = this match {
    case EmptyDiet()          =>  this
    case DietNode(rng, l, r) =>  {
      if (order.compare(x, rng.start) < 0) {
        DietNode(rng, l.remove(x), r)
      }
      else if (order.compare(x, rng.end) > 0) {
        DietNode(rng, l, r.remove(x))
      }
      else if (order.compare(x, rng.start) == 0) {
        if (order.compare(rng.start, rng.end) == 0)
          merge(l, r)
        else
          DietNode(Range(enum.succ(rng.start), rng.end), l, r)
      }
      else if (order.compare(x, rng.end) == 0) {
        DietNode(Range(rng.start, enum.pred(rng.end)), l, r)
      }
      else {
        DietNode(Range(rng.start, enum.pred(x)), l, DietNode(Range(enum.succ(x), rng.end), EmptyDiet(), r))
      }
    }
  }

  /**
    * remove a range from Diet
    */
  def removeRange(range: Range[A])(implicit enum: Enum[A], order: Order[A]): Diet[A] = this match {
    case EmptyDiet()             => this
    case DietNode(rng, l, r)  =>
      val left = if(order.lt(range.start, rng.start)) l.removeRange(range) else l
      val right = if(order.gt(range.end, rng.end)) r.removeRange(range) else r
        (rng - range) match {
        case None() => merge(l, r)
        case Some((m, None())) => DietNode(m, left, right)
        case Some((m, Some(n))) => merge(DietNode(m, l, EmptyDiet()), DietNode(n, EmptyDiet(), r))
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
  def ++(that: Diet[A])(implicit discrete:Enum[A], order: Order[A]): Diet[A] = that.foldLeftRange(this)((d, r) => d + r)

  /**
   * Returns the union of this Diet with another Diet.
   */
  def |(that: Diet[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] = this ++ that

  /**
   * intersection with the given range
   */
  def &(other: Range[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] =
    this match {
      case DietNode(range, left, right) =>
        val newLeft: Diet[A] = if(order.lt(other.start, range.start)) left & other else EmptyDiet()
        val newRight: Diet[A] = if(order.gt(other.end, range.end)) right & other else EmptyDiet()

        (range & other) match {
          case Some(range) =>       DietNode(range, newLeft, newRight)
          case None() => merge(newLeft, newRight)
        }
      case x => x
    }


  /**
   * intersection with the given diet
   */
  def &(that: Diet[A])(implicit discrete: Enum[A], order: Order[A]): Diet[A] =
    that.foldLeftRange(this)(_ & _)

  /**
   * min value in the tree
   */
  def min: Option[A] = this match {
    case EmptyDiet()  =>  None()
    case DietNode(Range(x, _), EmptyDiet(), _) => Some(x)
    case DietNode(_, l, _) => l.min
  }

  /**
   * max value in the tree
   */
  def max: Option[A] = this match {
    case EmptyDiet()  => None()
    case DietNode(Range(_, y), _, EmptyDiet()) => Some(y)
    case DietNode(_, _, r) => r.max
  }

  def map[B: Enum: Order](f: A => B): Diet[B] = this match {
    case EmptyDiet()          =>  Diet.empty[B]
    case DietNode(Range(a, b), l, r) =>  {
      val (lp, rp) = (l.map(f), r.map(f))

      val n = lp + f(a) + f(b)

      merge(n, rp)
    }
  }

  def foldRight[B](s: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit enumA: Enum[A], orderA: Order[A]): Eval[B] = this match {
    case EmptyDiet() => s
    case DietNode(rng, l, r) => l.foldRight(rng.toStreaming.foldRight(r.foldRight(s)(f))(f))(f)
  }

  def foldLeft[B](s: B)(f: (B, A) => B)(implicit enumA: Enum[A], orderA: Order[A]): B = this match {
    case EmptyDiet() =>  s
    case DietNode(rng, l, r) => r.foldLeft(rng.toStreaming.foldLeft(l.foldLeft[B](s)(f))(f))(f)
  }

  def foldLeftRange[B](z: B)(f: (B, Range[A]) => B): B = this match {
    case EmptyDiet() => z
    case DietNode(rng, l, r) => r.foldLeftRange(f(l.foldLeftRange(z)(f), rng))(f)
  }

  def foldRightRange[B](z: Eval[B])(f: (Range[A], Eval[B]) => Eval[B]): Eval[B] = this match {
    case EmptyDiet() => z
    case DietNode(rng, l, r) => l.foldRightRange(f(rng, r.foldRightRange(z)(f)))(f)
  }

  def toStreaming(implicit enum: Enum[A], order: Order[A]): Streaming[A] =
    this match {
      case EmptyDiet() => Streaming.empty
      case DietNode(rng,l,r) =>
        l.toStreaming ++ rng.toStreaming ++ r.toStreaming
    }
}

object Diet {
  def empty[A]: Diet[A] = EmptyDiet()

  private [dogs] case class DietNode[A](focus: Range[A], left: Diet[A], right: Diet[A]) extends Diet[A] {
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

      DietNode(Range(i._1, i._2), lp, r)
    }
  }

  private [dogs] def splitMax[A](n: Diet[A]): (Diet[A], (A, A)) = n match {
    case EmptyDiet() => throw new java.lang.Exception("can't happen")
    case DietNode(Range(x, y), l, EmptyDiet())   =>  (l, (x, y))
    case DietNode(rng, l, r)             =>  {
      val (d, i) = splitMax(r)
      (DietNode(rng,l, d), i)
    }
  }

  implicit def dietShowable[A](implicit s: Show[Range[A]]): Show[Diet[A]] = new Show[Diet[A]] {
    override def show(f: Diet[A]): String = f.foldLeftRange("{")((str,rng) => str + " " + s.show(rng)) + " }"
  }
}
