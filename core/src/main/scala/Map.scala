package dogs

import Predef._
import algebra.Order

/**
 * A tree based immutable Map.
 */
class Map[K,V](val set: Set[(K,V)]) {

  /**
   * Map a function on all the values in the Map.
   */
  def map[B](f: V => B)(implicit K: Order[K]): Map[K,B] =
    new Map(set.map(kv => kv._1 -> f(kv._2)))

  /**
   * Map a function on all the values in the Map.
   */
  def flatMap[B](f: V => Map[K,V])(implicit K: Order[K]): Map[K,V] =
    new Map(set.flatMap(kv => f(kv._2).set))

  /**
   * Fold across all the key/value pairs, associating minumum keys
   * first.
   */
  def foldLeft[B](b: B)(f: (B,(K,V)) => B): B =
    set.foldLeft(b)(f)

  /**
   * Check if we have the given key in the map.
   * O(log n)
   */
  def containsKey(key: K)(implicit K: Order[K]) = getkv(key).isDefined

  /**
   * Add a key value pair to the map.
   * O(log n)
   */
  def +(kv: (K,V))(implicit K: Order[K]): Map[K,V] = new Map(set + kv)

  /**
   * Get the value for the given key, if it exists.
   * O(log n)
   */
  def get(key: K)(implicit K: Order[K]): Option[V] = getkv(key).map(_._2)

  /**
   * Return a map which doesn't contain the given key.
   * O(log n)
   */
  def remove(key: K)(implicit K: Order[K]): Map[K,V] = new Map(set.removef(key, _._1))

  /**
   * Merge this Map with anohter Map.
   * O(n log n)
   */
  def ++(other: Map[K,V])(implicit K: Order[K]): Map[K,V] = new Map(set ++ other.set)

  // 
  // Fetch a Key/Value pair from the Map if the key is present.
  // O(log n)
  // 
  private def getkv(key: K)(implicit K: Order[K]): Option[(K,V)] =
    set.dothestupidthingbecausesetisnotamapdotbiz(_._1,  key)

  private implicit def order[X](implicit K: Order[K]): Order[(K,X)] = K.on[(K,X)](_._1)
}

object Map {
  /**
   * Construct a map containing the given key/value pairs.
   * O(n log n)
   */
  def apply[K,V](kvs: (K,V)*)(implicit K: Order[K]): Map[K,V] =
    kvs.foldLeft[Map[K,V]](empty)(_ + _)

  /**
   * Return an empty map.
   */
  def empty[K,V]: Map[K,V] = new Map(Set.empty)
}
