package cats

package object collections {

  @deprecated("Set has been renamed to AvlSet to avoid naming clashes.", "cats-collections 0.7.0")
  type Set[K] = AvlSet[K]
  @deprecated("Set has been renamed to AvlSet to avoid naming clashes.", "cats-collections 0.7.0")
  val Set = AvlSet

  @deprecated("Map has been renamed to AvlMap to avoid naming clashes.", "cats-collections 0.7.0")
  type Map[K, V] = AvlMap[K, V]
  @deprecated("Map has been renamed to AvlMap to avoid naming clashes.", "cats-collections 0.7.0")
  val Map = AvlMap

}
