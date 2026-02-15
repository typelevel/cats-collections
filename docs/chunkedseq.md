# ChunkedSeq

`ChunkedSeq` is an immutable sequence backed by a balanced tree of
contiguous array chunks. It combines the O(1) prepend, append, and
concatenation of `Chain` with cache-friendly O(N) iteration and O(log N)
indexed access.

## Motivation

Scala's standard library offers `List` (fast prepend, slow append and
index) and `Vector` (fast index, slow concat). The cats ecosystem adds
`Chain` (fast concat, but no index and poor iteration cache locality).
`ChunkedSeq` fills the gap: a structure you can build cheaply via
prepend, append, and concat, then consume efficiently via iteration or
random access.

Internally, elements live in bounded-size array chunks (32 elements by
default) at the leaves of a balanced binary tree. Sequential iteration
visits contiguous memory within each chunk, giving substantially better
cache behavior than tree-only structures. The balanced tree spine keeps
indexed access, take, drop, and updated all at O(log N).

All operations are stack-safe.

## Supported Operations

| Operation                        | Complexity          |
|----------------------------------|---------------------|
| prepend (`::`), append (`:+`)    | O(1)                |
| concat (`++`)                    | O(1)                |
| uncons, unsnoc                   | O(log N) amortized  |
| headOption, lastOption           | O(log N)            |
| get, getUnsafe                   | O(log N)            |
| take, drop, updated              | O(log N)            |
| foldLeft, foldRight, toIterator  | O(N)                |
| map, flatMap, filter, reverse    | O(N)                |
| size, isEmpty                    | O(1)                |

## Example Usage

Start by importing the library:

```scala mdoc:silent
import cats._, cats.implicits._, cats.collections._
```

Create a `ChunkedSeq` in several ways:

```scala mdoc
val empty = ChunkedSeq.empty[Int]
empty.isEmpty

val one = 42 :: ChunkedSeq.empty[Int]
one.headOption

val fromList = ChunkedSeq.fromList(List(1, 2, 3, 4, 5))
fromList.size
```

Prepend and append are both O(1):

```scala mdoc
val a = 0 :: fromList
val b = fromList :+ 6
a.toList
b.toList
```

Concatenation is O(1) as well:

```scala mdoc
val left = ChunkedSeq.fromList(List(1, 2, 3))
val right = ChunkedSeq.fromList(List(4, 5, 6))
val both = left ++ right
both.toList
```

Indexed access is O(log N):

```scala mdoc
both.get(0L)
both.get(3L)
both.get(100L)
```

`uncons` and `unsnoc` decompose from either end:

```scala mdoc
both.uncons
both.unsnoc
```

Pattern matching with `NonEmpty`:

```scala mdoc
both match {
  case ChunkedSeq.NonEmpty(head, tail) => s"head=$head, remaining=${tail.size}"
  case _ => "empty"
}
```

## Comparison with Other Structures

| Structure   | Prepend | Append | Concat | Index    | Iteration cache locality |
|-------------|---------|--------|--------|----------|--------------------------|
| List        | O(1)    | O(N)   | O(N)   | O(N)     | Poor (pointer chasing)   |
| Vector      | O(~1)   | O(~1)  | O(N)   | O(~1)    | Good                     |
| Chain       | O(1)    | O(1)   | O(1)   | O(N)     | Poor (unbalanced tree)   |
| ChunkedSeq  | O(1)    | O(1)   | O(1)   | O(log N) | Good (array chunks)      |

`ChunkedSeq` is particularly useful when you need to:

- Build a sequence from many small appends or concatenations (e.g., collecting results)
- Then consume it via iteration or indexed lookup
- Support stack-safe traversal of very large sequences

## Cats Instances

`ChunkedSeq` provides instances for:

- `Monad`
- `Alternative`
- `Traverse`
- `CoflatMap`
- `FunctorFilter`
- `Eq`, `PartialOrder`, `Order`
- `Monoid`
- `Show`

```scala mdoc
val cs = ChunkedSeq.fromList(List(1, 2, 3))
cs.show

Monad[ChunkedSeq].pure(42).toList

cs.foldMap(_.toString)
```
