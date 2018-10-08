package cats.collections.compat

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

final class Factory[-A, +C](cbf: CanBuildFrom[Nothing, A, C]) {
  def newBuilder: Builder[A, C] = cbf.apply()
}

object Factory {
  implicit def factory[A, C](implicit cbf: CanBuildFrom[Nothing, A, C]): Factory[A, C] =
    new Factory(cbf)
}
