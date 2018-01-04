package dogs

import scala.collection.{ generic, mutable, immutable, concurrent, JavaConverters }
import scala.reflect.ClassTag

/** A memo is a recipe for creating a memoized version of a function.
  *
  * The contract is that you pass in a function and get back a new version of that function that
  * uses the specified mechanism to cache results. A `Memo` doesn't necessarily guarantee that a
  * function will be called only once for a given input: each memo will have its own behavior with
  * respect to this.
  *
  * This provides a separation of the way a function is memoized from the point
  * where it happens. This both reduces boilerplate calls to functions like
  * [[scala.collection.mutable.Map.getOrElseUpdate `getOrElseUpdate`]] and allows for the
  * possibility of switching out implementations dynamically or with a single change.
  *
  * @example
  * {{{
  * import dogs.Memo
  *
  * // Memoize a function directly
  * val fastValue = Memo.mutableHashMapMemo { s: String => ??? }
  *
  * // Abstract over memoization strategy
  * val memoize = Memo.concurrentTrieMapMemo
  * val getValue = memoize { a: Int => ??? }
  *
  * // Memoize an existing function
  * def myFunction(s: String): Int = ???
  * val myFastFunction = memoize(myFunction)
  * }}}
  *
  * @tparam A the upper bound of the domain values that this `Memo` can store
  * @tparam B the upper bound of the range values that this `Memo` can store
  *
  * @note Implementors should ensure that a `Memo` produces a new backing store each time
  * [[Memo.apply apply]] is called so that it cannot produce incorrect results if two functions
  * disagree about the output for a given input. The `Memo` is the recipe, not the data store.
  */
trait Memo[A, B] {
  def apply[A1 <: A, B1 <: B](fn: A1 => B1): A1 => B1
}

/** Companion object that provides common implementations of [[Memo]].
  *
  * @define threadSafe @note This implementation is thread safe.
  * @define notThreadSafe @note This implementation is '''not''' thread safe.
  */
object Memo {
  private[this] type MemoInstance[A, B] = A => B

  /** A [[Memo]] that does not actually memoize your data.
    *
    * This may be useful to allow you to decide at runtime, or with a localized change, that you
    * want to stop doing memoization of a certain function. The `Memo` abstracts over the type of
    * storage.
    *
    * $threadSafe
    */
  lazy val nilMemo: Memo[Any, Any] = new Memo[Any, Any] {
    def apply[A, B](fn: A => B): A => B = fn
  }

  /** A [[Memo]] that only remembers the last value producted.
    *
    * This may be useful for functions that are called many times sequentially on data that has
    * runs. E.g., A function that is always called with the current date may see the same domain
    * value many times in a row, and will not benefit from a memory longer than a single entry.
    *
    * This `Memo` is thread safe, but it produces an object on the heap for each new value of the
    * function. See [[Memo.mutableLastValueMemo]] for a single-threaded version that does not
    * create any heap garbage as it is used.
    *
    * $threadSafe
    */
  lazy val lastValueMemo: Memo[Any, Any] = new Memo[Any, Any] {
    def apply[A, B](fn: A => B): A => B = new MemoInstance[A, B] {
      private[this] var last: LastState[A, B] = NoLast
      def apply(a: A): B = {
        last match {
          case HaveLast(lastA, b) if lastA == a => b
          case _ =>
            val b = fn(a)
            last = HaveLast(a, b)
            b
        }
      }
    }
  }

  /* This is isomorphic to Option[(A, B)], but `Some[Tuple2]` requires two heap objects and
   * `HaveLast` requires one. These are used only by [[lastValueMemo]], but nesting them produces
   * extra object references that increase the memory requirements. */
  private[this] sealed trait LastState[+A, +B]
  private[this] case object NoLast extends LastState[Nothing, Nothing]
  private[this] case class HaveLast[A, B](a: A, b: B) extends LastState[A, B]

  /** A [[Memo]] that only remembers the last value producted.
    *
    * This may be useful for functions that are called many times sequentially on data that has
    * runs. E.g., A function that is always called with the current date may see the same domain
    * value many times in a row, and will not benefit from a memory longer than a single entry.
    *
    * Unlike [[Memo.lastValueMemo]], this `Memo` is '''not''' thread safe, but it does not produce
    * any additional heap objects.
    *
    * $notThreadSafe
    */
  lazy val mutableLastValueMemo: Memo[Any, Any] = new Memo[Any, Any] {
    def apply[A, B](fn: A => B): A => B = new MemoInstance[A, B] {
      private[this] var haveLast: Boolean = false
      private[this] var lastA: A = _
      private[this] var lastB: B = _

      def apply(a: A) = {
        if (haveLast && a == lastA) {
          lastB
        } else {
          val b = fn(a)
          haveLast = true
          lastA = a
          lastB = b
          b
        }
      }
    }
  }

  /** A [[Memo]] based on an array of a fixed size.
    *
    * This may be useful for memoizing int-domain functions where the inputs are small, bounded numbers.
    * This `Memo` cannot memoize `null` results and will recompute the result if a `null` is produced.
    *
    * If the input value ''i'' does not satisfy 0 ≤ ''i'' < ''n'', memoization will be bypassed.
    *
    * @param n the size of the array to use for memoization
    *
    * $notThreadSafe
    */
  def arrayMemo[B0 >: Null : ClassTag](n: Int): Memo[Int, B0] = new Memo[Int, B0] {
    def apply[A <: Int, B <: B0](f: A => B): A => B = new MemoInstance[A, B] {
      private[this] val m: Array[_ >: B] = new Array[B0](n)
      def apply(a: A): B = {
        if (a >= n || a < 0) {
          f(a)
        } else {
          m(a) match {
            case null =>
              val b = f(a)
              m(a) = b
              b
            case other =>
              other.asInstanceOf[B]
          }
        }
      }
    }
  }

  /** Build a [[Memo]] based on an [[scala.collection.immutable.Map immutable Map]].
    *
    * @param mapFactory the factory to use to build the empty map
    *
    * $threadSafe
    */
  def immutableMapMemo[CC[A, +B] <: immutable.Map[A, B] with immutable.MapLike[A, B, CC[A, B]]]
                      (mapFactory: generic.ImmutableMapFactory[CC]): Memo[Any, Any] = new Memo[Any, Any] {
    def apply[A, B](fn: A => B): A => B = new MemoInstance[A, B] {
      private[this] var m: immutable.Map[A, B] = mapFactory.empty
      def apply(a: A) = m.getOrElse(a, {
        val b = fn(a)
        m = m.updated(a, b)
        b
      })
    }
  }

  /** A [[Memo]] based on the default Scala immutable map.
    *
    * $threadSafe
    */
  lazy val immutableDefaultMapMemo: Memo[Any, Any] = immutableMapMemo(immutable.Map)

  /** A [[Memo]] based on an immutable list map.
    *
    * $threadSafe
    */
  lazy val immutableListMapMemo: Memo[Any, Any] = immutableMapMemo(immutable.ListMap)

  /** A [[Memo]] based on an immutable hash map.
    *
    * $threadSafe
    */
  lazy val immutableHashMapMemo: Memo[Any, Any] = immutableMapMemo(immutable.HashMap)

  /** Build a [[Memo]] based on a mutable map.
    *
    * @note the produced `Memo` will be thread safe if and only if the underlying collection is thread
    * safe.
    *
    * @param mapFactory The factory to use to create the backing store that memoizes a function.
    */
  def mutableMapMemo[CC[A, B] <: mutable.Map[A, B] with mutable.MapLike[A, B, CC[A, B]]]
                    (mapFactory: generic.MutableMapFactory[CC]): Memo[Any, Any] = new Memo[Any, Any] {
    def apply[A, B](fn: A => B): A => B = new MemoInstance[A, B] {
      private[this] val m: mutable.Map[A, B] = mapFactory.empty
      def apply(a: A) = m.getOrElseUpdate(a, fn(a))
    }
  }

  /** A [[Memo]] based on the default mutable map.
    *
    * $notThreadSafe
    */
  lazy val mutableDefaultMapMemo: Memo[Any, Any] = mutableMapMemo(mutable.Map)

  /** A [[Memo]] based on a mutable hash map.
    *
    * $notThreadSafe
    */
  lazy val mutableHashMapMemo: Memo[Any, Any] = mutableMapMemo(mutable.HashMap)

  /** A [[Memo]] based on a weak hash map. This will allow results to be culled if memory is under pressure.
    *
    * $notThreadSafe Use [[Memo.concurrentWeakHashMapMemo]] if you need the concurrent version.
    */
  lazy val mutableWeakHashMapMemo: Memo[Any, Any] = mutableMapMemo(mutable.WeakHashMap)

  /** A [[Memo]] based on a concurrent trie map.
    *
    * $threadSafe
    */
  lazy val concurrentTrieMapMemo: Memo[Any, Any] = mutableMapMemo(concurrent.TrieMap)

  /** A [[Memo]] based on a synchronized weak hash map.
    *
    * $threadSafe
    */
  lazy val concurrentWeakHashMapMemo: Memo[Any, Any] = mutableMapMemo(ConcurrentWeakHashMapBuilder)

  /** A `MapFactory` that creates a `WeakHashMap` that's wrapped with synchronization. */
  private[this] object ConcurrentWeakHashMapBuilder extends generic.MutableMapFactory[mutable.Map] {
    import JavaConverters._
    def empty[A, B]: mutable.Map[A, B] =
      java.util.Collections.synchronizedMap(new java.util.WeakHashMap[A, B]).asScala
  }
}
