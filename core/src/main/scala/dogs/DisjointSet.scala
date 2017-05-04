package dogs

import Predef._
import cats._
import cats.instances.all._
import scala.annotation.tailrec


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
  @tailrec
  def find(u: T)(implicit order: Order[T]): Option[T] = {
    parents.get(u) match {
      case None() => None()
      case Some(v) => if (u == v) Some(v) else find(v)
    }
  }

  /**
    * Given two elements u and v, return Some(find(u),find(v))
    * if find(u) and find(v) are both defined. Otherwise
    * return None().
    */
  private[this] def getParents(u: T, v: T)(implicit order: Order[T]): Option[(T, T)] = {
    (find(u), find(v)) match {
      case (Some(x), Some(y)) => Some((x, y))
      case _ => None()
    }
  }

  /**
    * Given two elements u and v, return Some(ranks(u),ranks(v))
    * if ranks(u) and ranks(v) are both defined. Otherwise
    * return None().
    */
  private[this] def getRanks(u: T, v: T)(implicit order: Order[T]): Option[(Int, Int)] = {
    (ranks.get(u), ranks.get(v)) match {
      case (Some(uRank), Some(vRank)) => Some((uRank, vRank))
      case _ => None()
    }
  }

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
        getRanks(x, y) match {
          case (Some(rank)) =>
            if (rank._1 < rank._2) {
              Some(new DisjointSet[T](parents.remove(x).updateAppend(x, y),
                ranks, nComponents - 1))
            }
            else if (rank._1 > rank._2)
              Some(new DisjointSet[T](parents.remove(y).updateAppend(y,x),
                ranks, nComponents -1))
            else {
              val newRank = rank._1 + 1
              Some(new DisjointSet[T](parents.remove(y).updateAppend(y,x),
                ranks.remove(x).updateAppend(x,newRank), nComponents - 1))
            }
          case _ => None()
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