# PairingHeap

`PairingHeap` is a purely functional heap with excellent empirical
performance. It supports O(1) `add` and `combine` (merge) operations,
making it particularly useful when heaps need to be frequently merged.

See the [Wikipedia article](https://en.wikipedia.org/wiki/Pairing_heap)
for more details on the data structure.

## Supported Operations

- `add`:              insert an element: O(1)
- `remove`:           remove the minimum element: O(log N) amortized
- `combine`:          merge two heaps: O(1)
- `minimumOption`:    get the minimum element without removing: O(1)
- `pop`:              remove and return the minimum element: O(log N) amortized
- `toList`:           sorted list of all elements: O(N log N)
- `size`:             number of elements: O(1)
- `isEmpty`:          check if the heap is empty: O(1)
- `exists`:           check if any element satisfies a predicate: O(N)
- `forall`:           check if all elements satisfy a predicate: O(N)
- `takeLargest`:      efficiently find the k largest elements: O(N log k)

## Comparison with Heap

Unlike the binary `Heap`, `PairingHeap` supports an efficient O(1)
`combine` (merge) operation, making it the better choice when merging
heaps is needed. The binary `Heap` can be constructed in O(N) time
via `heapify`, while `PairingHeap` builds in O(N) time via
`fromIterable`.

Both `Heap` and `PairingHeap` implement the `PartiallyOrderedSet`
typeclass.

## Example Usage

Start by creating an empty `PairingHeap`:

```scala mdoc
import cats._, cats.implicits._, cats.collections._

val empty = PairingHeap.empty[Int]

empty.isEmpty

empty.size
```

Add some elements:

```scala mdoc
val heap = PairingHeap.empty[Int] + 5 + 3 + 8 + 1

heap.size

heap.minimumOption

heap.show
```

Remove the minimum element:

```scala mdoc
val heap2 = heap.remove

heap2.minimumOption

heap2.show
```

Build a heap from an existing collection:

```scala mdoc
val fromList = PairingHeap.fromIterable(List(10, 4, 7, 2, 9))

fromList.minimumOption

fromList.toList
```

Merge two heaps efficiently with `combine`:

```scala mdoc
val left = PairingHeap.fromIterable(List(1, 3, 5))
val right = PairingHeap.fromIterable(List(2, 4, 6))

val merged = left.combine(right)

merged.toList
```

Find the 3 largest values from a large collection:

```scala mdoc
val items = List(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)

val top3 = PairingHeap.takeLargest(items, 3)

top3.toList
```
