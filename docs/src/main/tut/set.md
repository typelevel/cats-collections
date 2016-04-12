---
layout: default
title:  "Set"
source: "core/src/main/scala/Set.scala"
---
# Set

`Set` is a tree-based set which stores [Order](order)able elements in
a [AVL balanced binary tree](https://en.wikipedia.org/wiki/AVL_tree).

This set is an
[Extensional Set](https://en.wikipedia.org/wiki/Extensional_definition),
as opposed to [ISet](iset) which is an
[Intensional Set](https://en.wikipedia.org/wiki/Intensional_definition). Which
is to say that membership of the set is defined by enumerating the
members.

Let's see how we can use `Set`

```tut
import dogs._, dogs.Predef._, cats._, dogs.syntax.all._, algebra.std.int._, cats.implicits._, cats.std.int._, dogs.syntax.all._

```

The empty set

```tut
val empty = Set.empty[Int]
```

Adding elements to the set

```tut

val s = empty + 5 + 6 + 10

```

An important functionality of `Set` is the ability to be partitioned into equivalence classes based of a property `p` of
function `f`. 

```tut

import dogs.Partition._

val s = Set.fromList(List(1,2,3,4,5,6,7,8,9,10))

s.partition(x => x % 3)
```
We partitioned the set into three equivalence classes, in this case the once that correspond to congruence mod 3.

Let's see a different partition of the same set based on a more complicated property (function)

```tut

val s = Set.fromList(List(1,2,3,4,5,6,7,8,9,10))

val f = (x: Int) => if (x < 5) 0 else if (x == 5 || x == 6) 1 else 2

s.partition(f)

```

Note that the result is of type `List[(B, List[A])]` where `B` is the value of `f` on all element of the class `B`. In
other words, each value of the class `B` represents the same within the `Set` under the function `f`