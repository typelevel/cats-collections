package cats.collections
package syntax

import cats.{Eval,Foldable,Order,Semigroup}

trait FoldableSyntax {

  implicit def foldableSyntax[F[_]: Foldable, A](fa: F[A]): FoldableOps[F,A] =
    new FoldableOps(fa)
}

final class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
  def toDogsVector: Vector[A] =
    F.foldLeft[A, Vector[A]](fa, Vector.empty)(_ :+ _)

  def toDogsMap[K,V](implicit K: Order[K], ev: A =:= (K,V)): Map[K,V] = {
    F.foldLeft(fa, Map.empty[K,V])(_ + _)
  }

  def toDogsMultiMap[K,V](implicit K: Order[K], ev: A =:= (K,V), V: Semigroup[V]): Map[K,V] = {
    F.foldLeft(fa, Map.empty[K,V]){(m,a) =>
      val (k,v) = ev(a)
      m.updateAppend(k,v)
    }
  }

  def toStreaming: Streaming[A] =
    F.foldRight(fa, Eval.now(Streaming.empty[A])){ (a, ls) =>
      Eval.now(Streaming.cons(a, ls))
    }.value
}
