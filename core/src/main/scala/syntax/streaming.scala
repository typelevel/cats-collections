package dogs
package syntax

import cats.Eval
/**
 * Contains various Stream-specific syntax.
 *
 * To eanble this, do one of the following:
 *
 *   import dogs.implicits._
 *   import dogs.bedazzle.all._
 *   import dogs.bedazzle.streaming._
 *
 * This provides the %:: and %::: operators for constructing Streams
 * lazily, and the %:: extract to use when pattern matching on
 * Streams.
 */
trait StreamingSyntax {
  import Streaming._

  object %:: {
    def unapply[A](s: Streaming[A]): Option[(A, Eval[Streaming[A]])] = s.uncons
  }

  implicit def streamingOps[A](as: => Streaming[A]): StreamingOps[A] =
    new StreamingOps(Eval.always(as))

  final class StreamingOps[A](rhs: Eval[Streaming[A]]) {
    def %::(lhs: A): Streaming[A] = Cons(lhs, rhs)
    def %:::(lhs: Streaming[A]): Streaming[A] = lhs ++ rhs
  }
}
