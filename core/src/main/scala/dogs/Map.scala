package dogs

import Predef._
import cats._

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
  def flatMap[B](f: V => Map[K,B])(implicit K: Order[K]): Map[K,B] =
    new Map(set.map(kv =>
      f(kv._2).get(kv._1).map((kv._1, _))
    ).flatMap(_ match {
      case Some(x) => Set(x)
      case _ => Set.empty
    }))

   /**
   * Fold across all the key/value pairs, associating maximum keys
   * first.
   */
  def foldRight[B](b: Eval[B])(f: ((K,V),Eval[B]) => Eval[B]): Eval[B] =
    set.foldRight(b)(f)

  /**
   * Fold across all the key/value pairs, associating minimum keys
   * first.
   */
  def foldLeft[B](b: B)(f: (B,(K,V)) => B): B =
    set.foldLeft(b)(f)

   /**
   * Convenience function for updating or removing a mapping for a key, where the mapping may or may not preexist.
   * O(log n + log n).  Current implementation has constant factors which are unnecessary and may be improved in future.
   */
  def alter(k: K)(f: Option[V] => Option[V])(implicit K: Order[K]): Map[K, V] =
    f(get(k)) map { v => this + (k -> v) } getOrElse this

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
   * Merge this Map with another Map.
   * O(n log n)
   */
  def ++(other: Map[K,V])(implicit K: Order[K]): Map[K,V] = new Map(set ++ other.set)

  /**
   * Return a list of Key,Value pairs
   * O(N)
   */
  def toList: List[(K,V)] = {
    val lb = new ListBuilder[(K,V)]
    this.foldLeft(()){(_,kv) => val _ = lb += kv}
    lb.run
  }

  /**
   * Return a stream of Key,Value pairs
   * O(N)
   */
  def toStreaming: Streaming[(K,V)] =
    foldRight(Eval.now(Streaming.empty[(K,V)])){ (a, ls) =>
      Eval.now(Streaming.cons(a, ls))
    }.value
              
  /**
   * Return a scala.collection.immutable.map
   */
  def toScalaMap: scala.collection.immutable.Map[K,V] =
    foldLeft(scala.collection.immutable.Map.empty[K,V])((m,kv) => m + kv)


  /**
   * Update this map with the given key,value, if the Key is already
   * present, the value is combined with the already present value
   * using the provided Semigroup.
   * O(log n)
   */
  def updateAppend(key: K, value: V)(implicit K: Order[K], V: Semigroup[V]): Map[K,V] =
    new Map(set.updateKey(key, value))

  //
  // Fetch a Key/Value pair from the Map if the key is present.
  // O(log n)
  //
  private def getkv(key: K)(implicit K: Order[K]): Option[(K,V)] =
    set._getkv(_._1,  key)

  private implicit def order[X](implicit K: Order[K]): Order[(K,X)] = K.on[(K,X)](_._1)
}

object Map extends MapInstances {
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

  implicit def toShow[K, V](implicit sk: Show[K], sv: Show[V]): Show[Map[K, V]] = new Show[Map[K, V]] {
    override def show(f: Map[K, V]): scala.Predef.String = {
      val pairs = f.toList

      val result = pairs.foldLeft("{"){(b, p) => b + "[" + sk.show(p._1) + "-->" + sv.show(p._2) + "]\n"} + "}"

      result
    }
  }

}

trait MapInstances {
  import scala.util.{Either => SEither, Right => SRight, Left => SLeft}

  implicit def eqMap[K: Eq, V: Eq](implicit K: Eq[K], V: Eq[V]): Eq[Map[K,V]] = new Eq[Map[K,V]] {
    // TODO get rid of this once cats has it:
    implicit val tupleEq: Eq[(K,V)] = new Eq[(K,V)] {
      override def eqv(l: (K,V), r: (K,V)): Boolean =
        K.eqv(l._1, r._1) && V.eqv(l._2, r._2)
    }

    override def eqv(l: Map[K,V], r: Map[K,V]): Boolean =
      Streaming.streamEq[(K,V)].eqv(l.toStreaming, r.toStreaming)
  }


  implicit def flatMapMap[K](implicit K: Order[K]) = new FlatMap[ Map[K,?]] {
    private implicit def order[X](implicit K: Order[K]): Order[(K,X)] = K.on[(K,X)](_._1)

    override def flatMap[A,B](fa: Map[K,A])(f: A => Map[K,B]) : Map[K,B] = fa flatMap f

    override def map[A,B](fa: Map[K,A])(f: A => B) : Map[K,B] = fa map f

    override def tailRecM[A,B](a: A)(f: A => Map[K,SEither[A,B]]) : Map[K,B] = {
      @tailrec def extract(kv : (K, SEither[A,B])) : Set[(K,B)] = kv._2 match {
        case SLeft(a) =>
          f(a).get(kv._1) match {
            case Some(x) => extract(kv._1 -> x)
            case _ => Set.empty[(K,B)]
          }
        case SRight(b) =>
          Set[(K,B)](kv._1 -> b)
      }
      new Map[K,B](f(a).set.flatMap(extract))
    }
  }
}
