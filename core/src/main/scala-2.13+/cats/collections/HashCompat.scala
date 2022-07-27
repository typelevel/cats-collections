package cats.collections

private[collections] class HashCompat {
  // adapted from scala.util.hashing.MurmurHash3
  def unorderedHash[A](xs: IterableOnce[A])(implicit A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, mixLast, setSeed}

    var a, b, n = 0
    var c = 1
    val iterator = xs.iterator
    while (iterator.hasNext) {
      val x = iterator.next()
      val h = A.hash(x)
      a += h
      b ^= h
      c *= h | 1
      n += 1
    }
    var h = setSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}
