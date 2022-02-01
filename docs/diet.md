# Diet

`Diet` is a Discrete Interval Encoding Tree. It stores subset of types
that have a total order. The types are also required to have a
successor and predecessor generator.

The discrete interval encoding tree is based on the observation that
the set of integers { i \| a<=i<=b } can be perfectly represented by
the closed interval \[a, b\].

`Diet` is a binary search tree where each node contains a set of
continuous values. If a value is inserted and it fills the hole
between to nodes (sets) then the two set become one since all value in
the new set are consecutive.

## Best and Worst Case Analysis.

The best case is when there is no holes in the stored set, so the tree
contains only one node (a single interval). In this case, it has O(1)
space requirement, inserting, deleting and finding a value is in
constant time.

In the worst case scenario, there is a hole of size one (1) between
each node and each node is in fact a set on size one (1). Under these
circumstances, operations in the tree required O(n).


## Supported Operations

- add:					add a value to the tree
- addRange:				add the entire range to the tree
- remove:				remove a value from the tree
- removeRange:          remove a range from the tree
- contains:				verify is a value is on the tree
- containsRange:		verify that an interval is on the tree
- (`-`) operator:		remove a range from the tree
-  (`&`) operator:	    calculates the intersection with another `Diet`
- (`|`) or (`++`) operator:	calculates the union with another `Diet`
- min:					min value in the tree
- max:					max value in the tree
- intervals:			the list of all intervals (sets) in the tree. The sets are disjoint sets.
- toList: 				list of sorted values in the tree

## `Diet` is *showable* so we can call `show` on it.

## Example usage:

Start by creating an empty Diet:

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

val d: Diet[Int] = Diet.empty
d.isEmpty

d.show
```

And we can add an item:

```scala mdoc
val d2 = d.add(12)
d2.isEmpty

d2.show
```

And now we can check that it thinks 12 is in the set, but not other numbers

```scala mdoc
d2.contains(1)
d2.contains(12)
```

If we remove our only element, we get back to the empty Diet:

```scala mdoc
val d3 = d2.remove(12)
d3.isEmpty
```

Asking to remove an element not in the set is a noop:

```scala mdoc
val s = Diet.empty[Int].remove(10)

s.show
```

Diet excels at storing ranges, so there are also operations that work on ranges of values:

```scala mdoc:nest
val d = Diet.empty[Int].addRange(1 toIncl 20)
d.contains(21)
d.contains(20)
d.contains(10)

val d2 = d - (10 toIncl 12)

d2.show

d2.contains(10)
d2.containsRange(1 toIncl 5)
d2.containsRange(11 toIncl 15) // fails since not the entire range is contained
```

Given two Diets, we can find the union or the intersection:

```scala mdoc:nest
val d1 = Diet.empty[Int] + (5 toIncl 10)
val d2 = Diet.empty[Int] + (7 toIncl 12)
(d1 & d2).show
(d1 | d2).show
```
Asking to remove non existing range yields the same diet

```scala mdoc:nest
val d = Diet.empty[Int].addRange((5 toIncl 20))

val d1 = d.removeRange((1 toIncl 4))
d1.show
```

Asking to remove a range yields a new Diet without the range

```scala mdoc:nest
val d = Diet.empty[Int].addRange((5 toIncl 20)).addRange(22 toIncl 30)
val d2 = d.removeRange((5 toIncl 20))

d2.show
```

Asking to remove a sub-range splits the Diet

```scala mdoc:nest
val d = Diet.empty[Int].addRange((5 toIncl 20))
val d3 = d.removeRange((10 toIncl 15))

(10 toIncl 15).toList.forall { i => d3.contains(i) }
(5 toIncl 9).toList.forall {i => d3.contains(i) }
(16 toIncl 20).toList.forall {i => d3.contains(i) }
```

Adding a inverted range

```scala mdoc:nest
val d = Diet.empty[Int] + Range(20, 10)

d.show
```
