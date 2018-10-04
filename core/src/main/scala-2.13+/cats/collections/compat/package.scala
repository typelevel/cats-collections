package cats.collections

package object compat {
  type Factory[-A, +C] = scala.collection.Factory[A, C]
}
