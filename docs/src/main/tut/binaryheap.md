---
layout: "docs"
title:  "Binary Heap"
---
# Binary Heap

`Heap` is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because
 their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based on
 Vladimir Kostyukov's paper and it does support the basic operations on a heap without compromising performance.

## Supported Operations

- add:						add a value to the heap
- remove:					remove a value from the heap (it removes the `min` value)
- (`--`) operator:		    remove a value from the heap (it removes the `min` value)
- min:						min value in the heap (it does not remove it)

- toList: 				    list of sorted values in the heap

## Example usage:

Start by creating an empty `Heap`:

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

val h = Heap.empty[Int]
h.isEmpty

h.show
```

Asking for the min value of a empty heap

```scala mdoc
h.getMin
```

And we can add an item:

```scala mdoc
val h2 = h.add(12)
h2.isEmpty

h2.show
```

Let's add a new items and ask for the min value:


```scala mdoc
val h3 = h2 + 5 + 1 + 7
h3.getMin


h3.show
```
If we call `remove` it removes the min value:

```scala mdoc
val r = h3.remove

r.show
```

## Heap sub projects

 More complex implementation of Heap will be added as sub projects where each implementation can be used for specific 
 requirements
