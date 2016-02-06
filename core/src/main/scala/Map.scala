package dogs

import Predef._

/**
 * A tree based immutable Map.
 */
class Map[K,V](val set: Set[(K,V)]) {
  /**
   * Fetch a Key/Value pair from the Map if the key is present.
   * O(log n)
   */
  private def getkv(key: K)(implicit K: Order[K]): Option[(K,V)] = set.find(kv => K.eq(kv._1,key))

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

  private implicit def order(implicit K: Order[K]): Order[(K,V)] = K.contramap[(K,V)](_._1)
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
