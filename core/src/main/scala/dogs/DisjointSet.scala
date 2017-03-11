package dogs

import Predef._
import cats._
import cats.instances.all._


/**
  * A disjoint set is a data structure that keeps track of a set of
  * elements that are partitioned into a number of disjoint subsets.
  * Useful for finding the number of components within a graph.
  */
final class DisjointSet[T] private (parents: Map[T,T], ranks: Map[T,Int], nComponents: Int) {

  /**
    * Return the number of components.
    */
  def components: Int = nComponents

  /**
    * Finds the root parent of a given element.
    */
  def find(u: T)(implicit order: Order[T]): Option[T] =
    parents.get(u).flatMap(v=> if (u == v) Some(v) else find(v))

  /**
    * Given two elements u and v, return Some(find(u),find(v))
    * if find(u) and find(v) are both defined. Otherwise
    * return None().
    */
  private[this] def getParents(u: T, v: T)(implicit order: Order[T]) : Option[(T,T)] = for {
    x <- find(u)
    y <- find(v)
  } yield (x,y)


  /**
    * Given two elements u and v, return Some(ranks(u),ranks(v))
    * getRanks is invoked only if getParents(u,v) are both defined,
    * so the None() case need not be checked for.
    */
  private[this] def getRanks(u: T, v: T)(implicit order: Order[T]): Option[(Int, Int)] = for {
    x <- ranks.get(u)
    y <- ranks.get(v)
  } yield (x,y)


  /**
    * Join two elements of the set and return a new disjoint
    * set with the two combined - if both elements are in the
    * set. Otherwise return None().
    */
  def union(u: T, v: T)(implicit order: Order[T], T: Semigroup[T]): Option[DisjointSet[T]] = {
    getParents(u, v) match {
      case None() => None()
      case (Some((x, y))) if x == y => Some(this)
      case (Some((x, y))) if x != y =>
        val c = if (u==v) nComponents  else nComponents - 1
        getRanks(x, y) match {
          case (Some((xr,yr))) =>
            if (xr < yr) {
              Some(new DisjointSet[T](parents.remove(x).updateAppend(x, y), ranks,c))
            }
            else if (xr > yr)
              Some(new DisjointSet[T](parents.remove(y).updateAppend(y,x), ranks,c))
            else {
              val newRank = xr + 1
              Some(new DisjointSet[T](parents.remove(y).updateAppend(y,x),
                ranks.remove(x).updateAppend(x,newRank),c))
            }
        }
    }
  }
}

object DisjointSet {

  /**
    * Initialize the Disjoint-set from a set of elements.
    */
  def apply[T](set: Set[T])(implicit order: Order[T], T: Semigroup[T]) : DisjointSet[T] = {
    val parents = set.map(x=>(x,x)).foldLeft[Map[T,T]](Map.empty)(_+_)
    val ranks = set.map(x=>(x,1)).foldLeft[Map[T,Int]](Map.empty)(_+_)
    new DisjointSet[T](parents,ranks,set.size)
  }

}