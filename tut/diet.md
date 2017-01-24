---
layout: default
title:  "Diet"
source: "core/src/main/scala/Diet.scala"
---
# Diet

`Diet` is a Discrete Interval Encoding Tree. It stores subset of types that have a total order. The types are also required to have a successor and predecessor generator. 

The discrete interval encoding tree is based on the observation that the set of integers { i \| a<=i<=b } can be perfectly represented by the closed interval [a, b].

`Diet` is a binary search tree where each node contains a set of continuos values. If a value is inserted and it fills the hole between to nodes (sets) then the two set become one since all value in the new set are consecutive.

## Best and Worst Case Analysis.

The best case is when there is no holes in the stored set, so the tree contains only one node (a single interval). In this case, it has O(1) space requirement, inserting, deleting and finding a value is in constant time. 

In the worst case scenario, there is a hole of size one (1) between each node and each node is in fact a set on size one (1). Under these circumstances, operations in the tree required O(n). 


## Supported Operations

- add:						add a value to the tree
- addRange:				add the entire range to the tree
- remove:					remove a value from the tree
- removeRange:          remove a range from the tree
- contains:				verify is a value is on the tree
- containsRange:			verify that an interval is on the tree
- (`-`) operator:		remove a range from the tree
-  (`&`) operator:	calculates the intersection with another `Diet`
- (`|`) or (`++`) operator:	calculates the union with another `Diet`
- min:						min value in the tree
- max:						max value in the tree
- intervals:				the list of all intervals (sets) in the tree. The sets are disjoint sets.
- toList: 				list of sorted values in the tree

## `Diet` is *showable* so we can call `show` on it.

## Example usage:

Start by creating an empty Diet:

```scala
scala> import dogs._, dogs.Predef._, cats._, cats.implicits._, dogs.syntax.all._
import dogs._
import dogs.Predef._
import cats._
import cats.implicits._
import dogs.syntax.all._

scala> val d: Diet[Int] = Diet.empty
d: dogs.Diet[dogs.Predef.Int] = EmptyDiet

scala> d.isEmpty
res0: dogs.Predef.Boolean = true

scala> d.show
res1: String = { }
```

And we can add an item:

```scala
scala> val d2 = d.add(12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(Range(12,12),EmptyDiet,EmptyDiet)

scala> d2.isEmpty
res2: dogs.Predef.Boolean = false

scala> d2.show
res3: String = { [12, 12] }
```

And now we can check that it thinks 12 is in the set, but not other numbers

```scala
scala> d2.contains(1)
res4: dogs.Predef.Boolean = false

scala> d2.contains(12)
res5: dogs.Predef.Boolean = true
```

If we remove our only element, we get back to the empty Diet:

```scala
scala> val d3 = d2.remove(12)
d3: dogs.Diet[dogs.Predef.Int] = EmptyDiet

scala> d3.isEmpty
res6: dogs.Predef.Boolean = true
```

Asking to remove an element not in the set is a noop:

```scala
scala> val s = Diet.empty[Int].remove(10)
s: dogs.Diet[dogs.Predef.Int] = EmptyDiet

scala> s.show
res7: String = { }
```

Diet excels at storing ranges, so there are also operations that work on ranges of values:

```scala
scala> val d = Diet.empty[Int].addRange(1 to 20)
d: dogs.Diet[dogs.Predef.Int] = DietNode(Range(1,20),EmptyDiet,EmptyDiet)

scala> d.contains(21)
res8: dogs.Predef.Boolean = false

scala> d.contains(20)
res9: dogs.Predef.Boolean = true

scala> d.contains(10)
res10: dogs.Predef.Boolean = true

scala> val d2 = d - (10 to 12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(Range(1,9),EmptyDiet,DietNode(Range(13,20),EmptyDiet,EmptyDiet))

scala> d2.show
res11: String = { [1, 9] [13, 20] }

scala> d2.contains(10)
res12: dogs.Predef.Boolean = false

scala> d2.containsRange(1 to 5)
res13: dogs.Predef.Boolean = true

scala> d2.containsRange(11 to 15) // fails since not the entire range is contained
res14: dogs.Predef.Boolean = false
```

Given two Diets, we can find the union or the intersection:

```scala
scala> val d1 = Diet.empty[Int] + (5 to 10)
d1: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,10),EmptyDiet,EmptyDiet)

scala> val d2 = Diet.empty[Int] + (7 to 12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(Range(7,12),EmptyDiet,EmptyDiet)

scala> (d1 & d2).show
res15: String = { [7, 10] }

scala> (d1 | d2).show
res16: String = { [5, 12] }
```
Asking to remove non existing range yields the same diet

```scala
scala> val d = Diet.empty[Int].addRange((5 to 20))
d: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,20),EmptyDiet,EmptyDiet)

scala> val d1 = d.removeRange((1 to 4))
d1: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,20),EmptyDiet,EmptyDiet)

scala> d1.show
res17: String = { [5, 20] }
```

Asking to remove a range yields a new Diet without the range

```scala
scala> val d = Diet.empty[Int].addRange((5 to 20)).addRange(22 to 30)
d: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,20),EmptyDiet,DietNode(Range(22,30),EmptyDiet,EmptyDiet))

scala> val d2 = d.removeRange((5 to 20))
d2: dogs.Diet[dogs.Predef.Int] = DietNode(Range(22,30),EmptyDiet,EmptyDiet)

scala> d2.show
res18: String = { [22, 30] }
```

Asking to remove a subrange splits the Diet

```scala
scala> val d = Diet.empty[Int].addRange((5 to 20))
d: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,20),EmptyDiet,EmptyDiet)

scala> val d3 = d.removeRange((10 to 15)) 
d3: dogs.Diet[dogs.Predef.Int] = DietNode(Range(5,9),EmptyDiet,DietNode(Range(16,20),EmptyDiet,EmptyDiet))

scala> (10 to 15).toList.forall { i => d3.contains(i) }
res19: dogs.Predef.Boolean = false

scala> (5 to 9).toList.forall {i => d3.contains(i) }
res20: dogs.Predef.Boolean = true

scala> (16 to 20).toList.forall {i => d3.contains(i) }
res21: dogs.Predef.Boolean = true
```

Adding a inverted range

```scala
scala> val d = Diet.empty[Int] + Range(20, 10)
d: dogs.Diet[dogs.Predef.Int] = DietNode(Range(10,20),EmptyDiet,EmptyDiet)

scala> d.show
res22: String = { [10, 20] }
```
