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
- contains:				verify is a value is on the tree
- containsRange:			verify that an interval is on the tree
- (`-`) operator:		remove a range from the tree
-  (`&`) operator:	calculates the intersection with another `Diet`
- (`|`) or (`++`) operator:	calculates the union with another `Diet`
- min:						min value in the tree
- max:						max value in the tree
- intervals:				the list of all intervals (sets) in the tree. The sets are disjoint sets.
- toList: 				list of sorted values in the tree

## Example usage:

Start by creating an empty Diet:

```scala
scala> import dogs._, dogs.Predef._, dogs.std._, dogs.syntax.all._
import dogs._
import dogs.Predef._
import dogs.std._
import dogs.syntax.all._

scala> val d: Diet[Int] = Diet.empty
d: dogs.Diet[dogs.Predef.Int] = EmptyDiet

scala> d.isEmpty
res0: dogs.Predef.Boolean = true
```

And we can add an item:

```scala
scala> val d2 = d.add(12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(12,12,EmptyDiet,EmptyDiet)

scala> d2.isEmpty
res1: dogs.Predef.Boolean = false
```

And now we can check that it thinks 12 is in the set, but not other numbers

```scala
scala> d2.contains(1)
res2: dogs.Predef.Boolean = false

scala> d2.contains(12)
res3: dogs.Predef.Boolean = true
```

If we remove our only element, we get back to the empty Diet:

```scala
scala> val d3 = d2.remove(12)
d3: dogs.Diet[dogs.Predef.Int] = EmptyDiet

scala> d3.isEmpty
res4: dogs.Predef.Boolean = true
```

Asking to remove an element not in the set is a noop:

```scala
scala> Diet.empty[Int].remove(10)
res5: dogs.Diet[dogs.Predef.Int] = EmptyDiet
```

Diet excels at storing ranges, so there are also operations that work on ranges of values:

```scala
scala> val d = Diet.empty[Int].addRange(1 to 20)
d: dogs.Diet[dogs.Predef.Int] = DietNode(1,20,EmptyDiet,EmptyDiet)

scala> d.contains(21)
res6: dogs.Predef.Boolean = false

scala> d.contains(20)
res7: dogs.Predef.Boolean = true

scala> d.contains(10)
res8: dogs.Predef.Boolean = true

scala> val d2 = d - (10 to 12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(1,9,EmptyDiet,DietNode(13,20,EmptyDiet,EmptyDiet))

scala> d2.contains(10)
res9: dogs.Predef.Boolean = false

scala> d2.containsRange(1 to 5)
res10: dogs.Predef.Boolean = true

scala> d2.containsRange(11 to 15) // fails since not the entire range is contained
res11: dogs.Predef.Boolean = false
```

Given two Diets, we can find the union or the intersection:

```scala
scala> val d1 = Diet.empty[Int] + (5 to 10)
d1: dogs.Diet[dogs.Predef.Int] = DietNode(5,10,EmptyDiet,EmptyDiet)

scala> val d2 = Diet.empty[Int] + (7 to 12)
d2: dogs.Diet[dogs.Predef.Int] = DietNode(7,12,EmptyDiet,EmptyDiet)

scala> (d1 & d2).intervals
res12: dogs.List[dogs.Range[dogs.Predef.Int]] = List(Range(7,10))

scala> (d1 | d2).intervals
res13: dogs.List[dogs.Range[dogs.Predef.Int]] = List(Range(5,12))
```
