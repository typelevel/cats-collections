---
layout: default
title:  "Diet"
source: "core/src/main/scala/Diet.scala"
---
# Diet

`Heap` is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because
 their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based on
 VLADIMIR KOSTYUKOV paper and it does support the basic operations on a heap without compromising performance. 

## Supported Operations

- add:						add a value to the heap
- remove:					remove a value from the heap (it removes the `min` value)
- (`--`) operator:		    remove a value from the heap (it removes the `min` value)
- min:						min value in the heap (it does not remove it)

- toList: 				    list of sorted values in the heap

## Example usage:

Start by creating an empty `Heap`:

```tut
import dogs._, dogs.Predef._, cats._, cats.implicits._, dogs.syntax.all._

val h = Heap.empty[Int]
h.isEmpty

h.show
```

Asking for the min value of a empty heap

```tut
h.getMin
```

And we can add an item:

```tut
val h2 = h.add(12)
h2.isEmpty

h2.show
```

Let's add a new items and ask for the min value:


```tut
val h3 = h2 + 5 + 1 + 7
h3.getMin


h3.show
```
If we call `remove` it removes the min value:

```tut
val r = h3.remove

r.show
```

## Heap sub projects

 More complex implementation of Heap will be added as sub projects where each implementation can be used for specific 
 requirements
