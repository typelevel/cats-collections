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

package cats.collections

import cats._
import cats.data.State
import cats.data.State.get

import DisjointSets.Entry

class DisjointSets[T: Order] private (private val entries: AvlMap[T, Entry[T]]) {

  /**
   * Joins two disjoint sets if both are contained by this [[DisjointSets]]
   *
   * @param a
   *   Set `a`
   * @param b
   *   Set `b`
   * @return
   *   (new [[DisjointSets]] with updated state, `true` if Both labels are contained and joined )
   */
  def union(a: T, b: T): (DisjointSets[T], Boolean) = {
    import DisjointSets.{find => findSt}

    val result: Option[DisjointSets[T]] = {
      for {
        opa <- findSt(a) // Find `a` parent's label, track path compression
        opb <- findSt(b) // Find `b` parent's label, track path compression
        dsets <- get // Get the state (the compressed [[DisjointSets]])
      } yield for {
        pa <- opa // If `a` was part of the collection
        pb <- opb // As well as `b`...
        flatEntries = dsets.entries
        paEntry <- flatEntries.get(pa) // then their ranks are recovered...
        pbEntry <- flatEntries.get(pb)
      } yield {
        val ((parent, parentEntry), (child, childEntry)) = {
          // ... so it is possible to determine which one should be placed below
          // the other minimizing the resulting tree depth
          val parent_child = (pa -> paEntry, pb -> pbEntry)
          if (paEntry.rank >= pbEntry.rank) parent_child else parent_child.swap
        }
        new DisjointSets[T](
          flatEntries ++ AvlMap(
            child -> childEntry.copy(parent = parent),
            parent -> parentEntry.copy(rank = scala.math.max(parentEntry.rank, childEntry.rank + 1))
          )
        )
      }
    }.runA(this).value

    result.getOrElse(this) -> result.isDefined

  }

  /**
   * Checks whether or not a value is present in the disjoint sets collection
   * @param v
   *   label to be found within the data structure
   * @return
   *   Check result
   */
  def contains(v: T): Boolean = entries containsKey v

  /**
   * Find the label of the provided value.
   * @param v
   *   Value whose label is to be found
   * @return
   *   (new state, 'None' if the value doesn't exist, Some(label) otherwise)
   */
  def find(v: T): (DisjointSets[T], Option[T]) = {
    val newState = entries.get(v).flatMap { _ =>
      flattenBranch(v)
    }
    (newState.getOrElse(this), newState.flatMap { st => st.entries.get(v).map(_.parent) })
  }

  /**
   * Add a value to this datastructure
   * @param v
   *   Value to be added
   * @return
   *   New [[DisjointSets]] 's state.
   */
  def +(v: T): DisjointSets[T] = {
    if (entries containsKey v) this
    else new DisjointSets(entries + (v -> Entry(0, v)))
  }

  /**
   * Generates a map from labels to sets from the current [[DisjointSets]].
   */
  def toSets: (DisjointSets[T], AvlMap[T, AvlSet[T]]) =
    entries.foldLeft((this, AvlMap[T, AvlSet[T]]())) { case ((dsets, acc), (k, _)) =>
      val (newSt, Some(label)) = dsets.find(k)
      val updatedSet = acc.get(label).getOrElse(AvlSet.empty[T]) + k
      (newSt, acc + (label -> updatedSet))
    }

  private def flattenBranch(label: T, toPropagate: AvlMap[T, Entry[T]] = AvlMap.empty): Option[DisjointSets[T]] =
    entries.get(label).flatMap {
      case Entry(_, parent) if parent == label =>
        val newEntries = entries ++ toPropagate.map(_.copy(parent = label))
        Some(new DisjointSets(newEntries))
      case entry @ Entry(_, parent) =>
        flattenBranch(parent, toPropagate + (label -> entry))
    }

}

object DisjointSets extends DisjointSetsStates {

  def apply[T: Order](labels: T*): DisjointSets[T] = new DisjointSets[T](
    AvlMap(labels.map(l => l -> Entry(0, l)): _*)
  )

  private case class Entry[T](rank: Int, parent: T)
}

trait DisjointSetsStates {

  def find[T](v: T): State[DisjointSets[T], Option[T]] =
    State[DisjointSets[T], Option[T]](disjointSets => disjointSets.find(v))

  def union[T](a: T, b: T): State[DisjointSets[T], Boolean] =
    State[DisjointSets[T], Boolean](disjointSets => disjointSets.union(a, b))

  def toSets[T]: State[DisjointSets[T], AvlMap[T, AvlSet[T]]] =
    State[DisjointSets[T], AvlMap[T, AvlSet[T]]](disjointSets => disjointSets.toSets)
}
