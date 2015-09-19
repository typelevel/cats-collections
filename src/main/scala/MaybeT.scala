package dogs

import cats._
import cats.data._

/**
  * monad transformer for Maybe
  */
final case class MaybeT[F[_], A](run: F[Maybe[A]]) {
  self =>
  import Maybe._

  def map[B](f: A => B)(implicit F: Functor[F]): MaybeT[F, B] = new MaybeT[F, B](mapO(_ map f))

  def flatMap[B](f: A => MaybeT[F, B])(implicit F: Monad[F]): MaybeT[F, B] = new MaybeT[F, B](
    F.flatMap(self.run)(_.cata(f(_).run, F.pure(notThere)))
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): MaybeT[F, B] = new MaybeT[F, B](
    F.flatMap(self.run)(_.cata((a => F.map(f(a))(there)), F.pure(notThere)))
  )

  def foldLeft[B](b: B)(f: (B,A) => B)(implicit F: Foldable[F]): B = {
    F.foldLeft[Maybe[A],B](run, b)((b, a) => Foldable[Maybe].foldLeft[A,B](a,b)(f))
  }

  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] = {
    F.foldRight[Maybe[A], B](run, b)((a, b) => Foldable[Maybe].foldRight[A, B](a, b)(f))
  }

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[MaybeT[F, B]] = {
    G.map(F.traverse(run)(o => Traverse[Maybe].traverse(o)(f)))(MaybeT.apply)
  }

  def ap[B](f: => MaybeT[F, A => B])(implicit F: Monad[F]): MaybeT[F, B] =
    MaybeT(F.flatMap(f.run)(_.cata(ff => F.map(run)(_ map ff), F.pure(notThere))))

  /** Apply a function in the environment of both maybes, containing
    * both `F`s.  It is not compatible with `Monad#bind`.
    */
  def app[B](f: => MaybeT[F, A => B])(implicit F: Apply[F]): MaybeT[F, B] =
    MaybeT(F.map2(f.run, run) {
      case (ff, aa) => maybeInstance.ap(aa)(ff)
    })

  def isThere(implicit F: Functor[F]): F[Boolean] = mapO(_.isThere)

  def isDefined(implicit F: Functor[F]): F[Boolean] = mapO(_.isThere)

  def isNotThere(implicit F: Functor[F]): F[Boolean] = mapO(_.isNotThere)

  def filter(f: A => Boolean)(implicit F: Functor[F]): MaybeT[F, A] = MaybeT(F.map(self.run) { _ filter f })

  def cata[X](there: A => X, notThere: => X)(implicit F: Functor[F]): F[X] = mapO(_.cata(there, notThere))

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = mapO(_.getOrElse(default))

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] =
    F.flatMap(self.run)(_.cata(F.pure(_), default))

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.exists(f))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.forall(f))

  def orElse(a: => MaybeT[F, A])(implicit F: Monad[F]): MaybeT[F, A] =
    MaybeT(F.flatMap(run)(_.cata(a => F.pure(there(a)), a.run)))

  def toRight[E](e: => E)(implicit F: Functor[F]): XorT[F,E,A] = XorT(F.map(run)(_.toRight(e)))

  def toLeft[B](b: => B)(implicit F: Functor[F]): XorT[F,A,B] = XorT(F.map(run)(_.toLeft(b)))

  private def mapO[B](f: Maybe[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class MaybeTInstances2 {
  implicit def maybeTFunctor[F[_]](implicit F0: Functor[F]): Functor[MaybeT[F, ?]] =
    new MaybeTFunctor[F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class MaybeTInstances1 extends MaybeTInstances2 {
  implicit def maybeTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[MaybeT[F, ?]] =
    new MaybeTFoldable[F] {
      implicit def F: Foldable[F] = F0
    }
}

sealed abstract class MaybeTInstances0 extends MaybeTInstances1 {
  implicit def maybeTMonadPlus[F[_]](implicit F0: Monad[F]): MonadCombine[MaybeT[F, ?]] =
    new MaybeTMonadCombine[F] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class MaybeTInstances extends MaybeTInstances0 {

  implicit def maybeTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[MaybeT[F, ?]] =
    new MaybeTTraverse[F] {
      implicit def F: Traverse[F] = F0
    }

  implicit def maybeTEqual[F[_], A](implicit F0: Eq[F[Maybe[A]]]): Eq[MaybeT[F, A]] =
    F0.on((_: MaybeT[F, A]).run)
}

trait MaybeTFunctions {
  def maybeT[M[_]] = 
    new (λ[α => M[Maybe[α]]] ~> MaybeT[M, ?]) {
      def apply[A](a: M[Maybe[A]]) = new MaybeT[M, A](a)
    }

  def there[M[_], A](v: => A)(implicit M: Applicative[M]): MaybeT[M, A] =
    MaybeT.maybeT[M].apply[A](M.pure(Maybe.there(v)))

  def notThere[M[_], A](implicit M: Applicative[M]): MaybeT[M, A] =
    MaybeT.maybeT[M].apply[A](M.pure(Maybe.notThere))

}

object MaybeT extends MaybeTInstances with MaybeTFunctions

//
// Implementation traits for type class instances
//

private trait MaybeTFunctor[F[_]] extends Functor[MaybeT[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: MaybeT[F, A])(f: A => B): MaybeT[F, B] = fa map f
}

private trait MaybeTMonad[F[_]] extends Monad[MaybeT[F, ?]] with MaybeTFunctor[F] {
  implicit def F: Monad[F]

  override def ap[A, B](fa: MaybeT[F, A])(f: MaybeT[F, A => B]): MaybeT[F, B] = fa ap f

  override def pure[A](a: A): MaybeT[F, A] = MaybeT[F, A](F.pure(Maybe.there(a)))

  override def flatMap[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] = fa flatMap f

}

private trait MaybeTFoldable[F[_]] extends Foldable[MaybeT[F, ?]] {
  implicit def F: Foldable[F]

  override def foldLeft[A,B](fa: MaybeT[F, A], b: B)(f: (B,A) => B): B =
    fa.foldLeft(b)(f)
    

  override def foldRight[A, B](fa: MaybeT[F, A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(b)(f)
}

private trait MaybeTTraverse[F[_]] extends Traverse[MaybeT[F, ?]] with MaybeTFoldable[F] with MaybeTFunctor[F]{
  implicit def F: Traverse[F]

  def traverse[G[_] : Applicative, A, B](fa: MaybeT[F, A])(f: A => G[B]): G[MaybeT[F, B]] = fa traverse f
}

private trait MaybeTMonadCombine[F[_]] extends MonadCombine[MaybeT[F, ?]] with MaybeTMonad[F] {
  implicit def F: Monad[F]

  def empty[A]: MaybeT[F, A] = MaybeT(F pure Maybe.notThere)
  def combine[A](a: MaybeT[F, A], b: MaybeT[F, A]): MaybeT[F, A] = a orElse b
}
