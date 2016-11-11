---
layout: default
title:  "List"
source: "core/src/main/scala/List.scala"
---
# List

`List` is a singly linked list, just as `scala.List` with some key differences:

- `dogs.List` isn't part of [an insane subtype heirarchy](https://tpolecat.github.io/2013/11/13/list.html)
- `dogs.List` is invariant instead of covariant,

## Using List

### Constructing Lists

A list can be constructed from the List.apply method:

```tut
List(1,2,3,4,5)
```

#### Empty lists

The preferred way to construct an empty list with the `empty` member of the `List` companion object:

```tut
import dogs._, Predef._

val x = List.empty[Int]
val y: List[Int] = List.empty
```

#### Non-empty Lists

You can create a non-empty list by prepending to another list, similar to `scala.List`:

```tut
val x = 1 :: List.empty[Int]
```

Note that the type returned here is `dogs.Nel`. This is different than
what happens with the standard library `::`, which returns `List`:

```tut
val x = 1 :: scala.Nil
```


### Pattern matching

The pattern `El()` will match empty lists, The parentheses are
required here, for a similar reason that they are required on
[Option](option)'s `None()` pattern.

`Nel(head, tail)` will match on the head and tail of a non-empty list.

```tut
import List._

def listPrint[A](l: List[A]) = l match {
  case El() => println("empty")
  case Nel(h,t) => println(s"Non-empty, with $h at head")
}

listPrint(empty)
listPrint(1 :: empty)
```

### Interoperating with the standard library

There is a method for converting a list to a `scala.collection.immutable.List`

```tut
val x: dogs.List[Int] = List(1,2,3,4,5)
val y = x.toScalaList
```

There is also a convenience method to create a list from anything implementing the Iterable interface:

```tut
val x = List.fromIterable(scala.collection.immutable.Vector(1,2,3))
val y = List.fromIterable(scala.Range(1, 10))
val z = List.fromIterable(scala.collection.immutable.List(1,2,3))
```

### Methods on List

Many of the expected methods on `List` exist. We are still missing
many other useful methods, please feel free to contribute!

```tut
import cats._
import cats.implicits._

val x: List[Int] = List(1,2,3)

x.map(_.toString)
x.flatMap(x => List(x, x+1))
x.filter(_ % 2 == 0)
x.foldLeft(0)(_ + _)
x.foldRight(Eval.now(scala.Vector.empty[Int]))((a,la) => la.map(a +: _))
x.foreach(println)
x.headOption
x ++ List(6,7,8)
x.find(_ == 7)
x.find(_ % 3 == 1)
x.contains(2)
x.forall(_ < 10)
x.exists(_ < 10)
x.reverse
x.take(2)
x.drop(1)
x.isEmpty
```
