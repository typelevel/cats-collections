package dogs

import Predef._
import cats._

import DisjointSets.Entry

class DisjointSets[T : Order] private(private val entries: Map[T, Entry[T]]) {

  /**
    * Joins two disjoint sets if both are contained by this [[DisjointSets]]
    *
    * @param a Set `a`
    * @param b Set `b`
    * @return (new [[DisjointSets]] with updated state,
    *   `true` if Both labels are contained and joined
    * )
    */
  def union(a: T, b: T): (DisjointSets[T], Boolean) = {

    val result: Option[DisjointSets[T]] = {
      val (phase1, opa) = find(a)
      for {
        pa <- opa
        (phase2, opb) = phase1.find(b)
        pb <- opb
      } yield {
        val Some(((parent, parentEntry), (child, childEntry))) = {
          import phase2.{entries => flatEntries}
          for {
            paEntry <- flatEntries get pa
            pbEntry <- flatEntries get pb
          } yield {
            val parent_child = (pa -> paEntry, pb -> pbEntry)
            if(paEntry.rank >= pbEntry.rank) parent_child else parent_child.swap
          }
        }
        new DisjointSets[T] (
          phase2.entries ++ Map(
            child -> childEntry.copy(parent = parent),
            parent -> parentEntry.copy(rank = scala.math.max(parentEntry.rank, childEntry.rank+1))
          )
        )
      }
    }
    result.getOrElse(this) -> result.isDefined

  }

  /**
    * Checks whether or not a value is present in the disjoint sets collection
    * @param v label to be found within the data structure
    * @return Check result
    */
  def contains(v: T): Boolean = entries containsKey v

  /**
    * Find the label of the provided value.
    * @param v Value whose label is to be found
    * @return (new state, 'None' if the value doesn't exist, Some(label) otherwise)
    */
  def find(v: T): (DisjointSets[T], Option[T]) = {
    val newState = entries.get(v) map { _ =>
      flattenBranch(v)
    }
    (newState.getOrElse(this), newState flatMap { st => st.entries.get(v).map(_.parent) })
  }

  /**
    * Add a value to this datastructure
    * @param v Value to be added
    * @return New [[DisjointSets]]'s state.
    */
  def +(v: T): DisjointSets[T] = {
    if(entries containsKey v) this
    else new DisjointSets(entries + (v -> Entry(0, v)))
  }

  /**
    * Generates a map from labels to sets from
    * the current [[DisjointSets]].
    */
  def toSets: (DisjointSets[T], Map[T, Set[T]]) =
    entries.foldLeft((this, Map[T, Set[T]]())) {
      case ((dsets, acc), (k, _)) =>
        val (newSt, Some(label)) = dsets.find(k)
        val updatedSet = acc.get(label).getOrElse(Set.empty[T]) + k
        (newSt, acc + (label -> updatedSet))
    }

  private def flattenBranch(label: T, toPropagate: Map[T, Entry[T]] = Map.empty): DisjointSets[T] =
    entries.get(label) match {
      case Some(Entry(_, parent)) if parent == label =>
        val newEntries = entries ++ toPropagate
        new DisjointSets(newEntries)
      case Some(entry @ Entry(_, parent)) => flattenBranch(parent, toPropagate + (label -> entry))
    }

}

object DisjointSets {

  def apply[T : Order](labels: T*): DisjointSets[T] = new DisjointSets[T](
    Map(labels.map(l => l -> Entry(0, l)):_*)
  )

  private case class Entry[T](rank: Int, parent: T)
}
