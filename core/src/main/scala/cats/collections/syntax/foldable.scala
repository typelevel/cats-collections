package cats.collections
package syntax

import cats.{Eval,Foldable,Order,Semigroup}

trait FoldableSyntax {

  implicit def foldableSyntax[F[_]: Foldable, A](fa: F[A]): FoldableOps[F,A] =
    new FoldableOps(fa)
}

final class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
  @deprecated("Dogs has been renamed to cats-collections, use toCatsVector instead", "cats-collections 0.7.0")
  def toDogsVector: Vector[A] = toCatsVector

  def toCatsVector: Vector[A] =
    F.foldLeft[A, Vector[A]](fa, Vector.empty)(_ :+ _)

  @deprecated("Dogs has been renamed to cats-collections, use toCatsMap instead", "cats-collections 0.7.0")
  def toDogsMap[K,V](implicit K: Order[K], ev: A =:= (K,V)): AvlMap[K,V] = toCatsMap[K,V]

  def toCatsMap[K,V](implicit K: Order[K], ev: A =:= (K,V)): AvlMap[K,V] = {
    F.foldLeft(fa, AvlMap.empty[K,V])(_ + _)
  }

  @deprecated("Dogs has been renamed to cats-collections, use toCatsMultiMap instead", "cats-collections 0.7.0")
  def toDogsMultiMap[K,V](implicit K: Order[K], ev: A =:= (K,V), V: Semigroup[V]): AvlMap[K,V] = toCatsMultiMap[K,V]

  def toCatsMultiMap[K,V](implicit K: Order[K], ev: A =:= (K,V), V: Semigroup[V]): AvlMap[K,V] = {
    F.foldLeft(fa, AvlMap.empty[K,V]){(m,a) =>
      val (k,v) = ev(a)
      m.updateAppend(k,v)
    }
  }

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  def toStreaming: Streaming[A] =
    F.foldRight(fa, Eval.now(Streaming.empty[A])){ (a, ls) =>
      Eval.now(Streaming.cons(a, ls))
    }.value
}
