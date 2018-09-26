package cats.collections
package syntax

import cats.Eval

@deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
trait StreamingSyntax {
  import Streaming._

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  object %:: {
    @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
    def unapply[A](s: Streaming[A]): Option[(A, Eval[Streaming[A]])] = s.uncons
  }

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  implicit def streamingOps[A](as: => Streaming[A]): StreamingOps[A] =
    new StreamingOps(Eval.always(as))

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  final class StreamingOps[A](rhs: Eval[Streaming[A]]) {
    @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
    def %::(lhs: A): Streaming[A] = Cons(lhs, rhs)
    @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
    def %:::(lhs: Streaming[A]): Streaming[A] = lhs ++ rhs
  }
}
