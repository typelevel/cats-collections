---
layout: default
title:  "Partition"
source: "core/src/main/scala/Partition.scala"
---------------------------------------------
# Sorted

`Partition` is a type class for adding partitioning functionality to `List[A]`
## Supported Operations

- partition:				Partition the list based on the predicate 

## Example usage:

If we have a `List` we can easily partition it:

```tut
import dogs._, dogs.Predef._, cats._, dogs.syntax.all._, algebra.std.int._, cats.implicits._, cats.std.int._, dogs.syntax.all._

val l = List(5,3,4,1,2,6,8,7,6,4)
val (even, odd) = l.partition(i => i % 2 == 0)

even.show
odd.show

```

`Partition` is sometimes used for optimizing sorts and searches.