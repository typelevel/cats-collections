---
layout: default
title:  "DisjointSet"
source: "core/src/main/scala/DisjointSet.scala"
---
# Disjoint-set

`Disjoint-set` is a basic data structure that tracks which partition an element in the set belong to, see
 [Disjoint-set data structure](https://en.wikipedia.org/wiki/Disjoint-set_data_structure).
 It is often used as a building block for graph data structures or functions.
 

 There are just a few basic operations:
 
 - `def find(x)`:	Returns the 'parent' of `x` (i.e. which partition it belong to).
 - `def union(x, y)`: 	Joins two subsets into a single subset.
 - `numComponents`: The number of partitions in the set.
