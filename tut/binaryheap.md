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

```scala
scala> import dogs._, dogs.Predef._, cats._, cats.implicits._, dogs.syntax.all._
import dogs._
import dogs.Predef._
import cats._
import cats.implicits._
import dogs.syntax.all._

scala> val h = Heap.empty[Int]
h: dogs.Heap[dogs.Predef.Int] = Leaf

scala> h.isEmpty
res0: dogs.Predef.Boolean = true

scala> h.show
res1: String = []
```

Asking for the min value of a empty heap

```scala
scala> h.getMin
res2: dogs.Option[dogs.Predef.Int] = None
```

And we can add an item:

```scala
scala> val h2 = h.add(12)
h2: dogs.Heap[dogs.Predef.Int] = Branch(12,Leaf,Leaf,1,1)

scala> h2.isEmpty
res3: dogs.Predef.Boolean = false

scala> h2.show
res4: String = [12]
```

Let's add a new items and ask for the min value:


```scala
scala> val h3 = h2 + 5 + 1 + 7
h3: dogs.Heap[dogs.Predef.Int] = Branch(1,Branch(7,Branch(12,Leaf,Leaf,1,1),Leaf,2,2),Branch(5,Leaf,Leaf,1,1),4,3)

scala> h3.getMin
res5: dogs.Option[dogs.Predef.Int] = Some(1)

scala> h3.show
res6: String = [1, 5, 7, 12]
```
If we call `remove` it removes the min value:

```scala
scala> val r = h3.remove
r: dogs.Heap[dogs.Predef.Int] = Branch(5,Branch(7,Leaf,Leaf,1,1),Branch(12,Leaf,Leaf,1,1),3,2)

scala> r.show
res7: String = [5, 7, 12]
```

## Heap sub projects

 More complex implementation of Heap will be added as sub projects where each implementation can be used for specific 
 requirements
