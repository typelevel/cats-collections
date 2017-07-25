---
layout: default
title:  "DisjointSets"
source: "core/src/main/scala/dogs/DisjointSets.scala"
---
# Disjoint Sets

`DisjointSets` provides a purely functional implementation for the union-find collection. 
An Union-Find (aka Disjoint Sets) structure is a set of sets where the intersection of any two sets is empty.

![](http://i.imgur.com/K7BwoOk.png)

![](http://i.imgur.com/SkqEdwt.png)

This constraint opens the way to fast unions (`O(1)` average). That makes of Disjoint Sets the perfect tool for
 clustering algorithms such as calculating the connected components in a graph.
 
Initially, it is a flat collection where each element forms its own, size 1, disjoint set.
New elements are added as new disjoint sets and union operations can be used to fusion these sets.
The joined sets are represented by one of its elements known as ***label* or *root***. 

Fast fusion of disjoints sets is key and its provided through parenthood relations. Sets `labels` are always the 
top-level ancestor.

The following example shows 3 disjoint sets of size 1.

![](http://i.imgur.com/h8ddOkT.png)

Whereas, in the next one, `C` is the parent of the `{A, B, C}` set which is the only set in the DisjointSets structure. 

![](http://i.imgur.com/V71Z0eS.png)

## Supported operations

- `dsets + c` (add): Adds a value as a single element set:

![](http://i.imgur.com/iA00VgQ.png)

- `desets.union(A,C)` (union/join): Fusion two disjoint sets:

![](http://i.imgur.com/7uunsNJ.png)


- `dsets.find(v)` (find): Retrieves `Some(label)` if `v` belongs to the set with that label or `None` if the 
value is not part of `dsets`.

- `dsets.toSets`: Gets a `Map[T, Set[T]]` representation of the DisjointSets contents where the key is each set 
*label*. 
 
## Example usage

```scala
import DisjointSets._

val operations = for {
  _ <- union(1,2)
  oneAndTwo <- find(2)
  _ <- union(3,4)
  threeAndFour <- find(3)
  _ <- union(2,3)
  sets <- toSets
} yield sets

val label2disjointset: Map[Int, Set[Int]] = operations.runA(DisjointSets(1,2,3,4)).value
```

## Structure and performance 

The main idea is that each element starts as a disjoint set itself. A set with two or more elements is always the result of one or several _union_ operations. Thus, a multi-element set is composed of sets of just one element, call them components. 

Each component has 3 fields:

- One for the value of the element it contains.
- A reference pointing to another component of the same composed multi-element set. Or itself if it constitutes a single element set.
- An estimation of how many components/nodes compose its descendants. This estimation is known as **rank**.


Let’s assume that the next operations are executed:

```
dsets.union(B,A) //1
dsets.union(B,C) //2
```

From a mathematical point of view, the result should be similar to the one shown below:

![](http://i.imgur.com/V71Z0eS.png)

However, in order to improve lookups performance, some optimizations need to be applied. Therefore, with optimized 
operations, the resulting structure after (1) and (2) is:

![](http://i.imgur.com/9srckn2.png)

Each set is a tree represented by its _root_. Therefore, looking for the set an element belongs to is not more than 
following its parental relations until the root of the tree is reached. So the shallower the tree is, the fewer
the operations to be performed in the lookup are.

On the other hand, the operation where two sets are merged (_union_) consist on making one of the two trees to become a 
branch of the other:

![](http://i.imgur.com/aKHPrtV.png)


### Heuristics

These are two optimizations whose goal is to reduce the depth of each tree representing a set. The lower the tree height is, 
the fewer operations are needed to find one element set  label.

Considering `DisjointSets` structure as a forest of tree-sets, let’s compute ![](http://i.imgur.com/wgbq86Y.png) as the maximum tree height of the 
forest.
The maximum number of operations to complete before finding one element in one set is proportional to the number of 
jumps from the deepest leaf to its tree root. That is ![](http://i.imgur.com/DR5IUP3.png).

There are two heuristics designed to keep these tress low:

- **Union by rank:**  Avoids incrementing ![](http://i.imgur.com/DR5IUP3.png) whenever possible. The idea is for each 
value node to store how many levels have the sub-tree that they are ancestor of (call it rank). All new elements 
have a rank value of 0 (the _root_ is the first level). This way, whenever an _union_ operation is 
executed, is the shorter tree the one that becomes a branch of the other and, therefore, ![](http://i.imgur.com/DR5IUP3.png) does not increase.
- **Path compression:** In this case the aim is to take advantage of the jumps made in _find_ operation. Whenever a tree 
_root_ is found it is assigned to every node in the search path. This is a purely functional data structure so the 
changes are reflected in a new copy of it which forms part of the operation result.
