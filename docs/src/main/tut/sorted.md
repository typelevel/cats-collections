---
layout: default
title:  "Sorted"
source: "core/src/main/scala/Diet.scala"
---
# Sorted

`Sorted` is a type class for adding sorting functionality to `List[A]`
## Supported Operations

- quicksort:				sorted the list using Quick Sort algorithm
- heap sort:				sorted the list using Heap Sort algorithm 

## Example usage:

If we have a `List` we can easily sort it:

```tut
import dogs._, dogs.Predef._, cats._, dogs.syntax.all._, algebra.std.int._, cats.implicits._, cats.std.int._, dogs.syntax.all._

val l = List(5,3,4,1,2,6,8,7,6,4)
l.sorted.show

```

`List.sorted` will use quick sort by default, but we can use heapsort
if we want to. 

```tut
Sorted.heapSort(l).sorted.show
```