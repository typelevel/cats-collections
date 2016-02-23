---
layout: default
title:  "Diet"
source: "core/src/main/scala/Diet.scala"
---
# Diet

`Diet` is a Discrete Interval Encoding Tree. It stores subset of types that have a total order. The types are also required to have a successor and predecessor generator. 

The discrete interval encoding tree is based on the observation that the set of integers { i| a<=i<=b } can be perfectly represented by the closed interval [a, b].

`Diet` is a binary search tree where each node contains a set of continuos values. If a value is inserted and it fills the hole between to nodes (sets) then the two set become one since all value in the new set are consecutive.

## Best and Worst Case Analysis.

The best case is when there is no holes in the stored set, so the tree contains only one node (a single interval). In this case, it has ***O***(1) space requirement, inserting, deleting and finding a value is in constant time. 

In the worst case scenario, there is a hole of size one (1) between each node and each node is in fact a set on size one (1). Under these circumstances, it behaves as a regular binary search tree. 

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


