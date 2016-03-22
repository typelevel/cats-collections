---
layout: default
title:  "List"
source: "core/src/main/scala/List.scala"
---
# List

`List` is a singly linked list, just as `scala.List` with some key differences:

- `dogs.List` isn't insn't part of [an insane subtype heirarchy](https://tpolecat.github.io/2013/11/13/list.html)
- `dogs.List` is invariant instead of covariant,

## Using List

### Constructing Lists

A list can be constructed from the List.apply method:

```scala
scala> List(1,2,3,4,5)
res0: List[Int] = List(1, 2, 3, 4, 5)
```

#### Empty lists

The preferred way to construct an empty list with the `empty` member of the `List` companion object:

```scala
scala> import dogs._, Predef._
import dogs._
import Predef._

scala> val x = List.empty[Int]
x: dogs.List[dogs.Predef.Int] = El()

scala> val y: List[Int] = List.empty
y: dogs.List[dogs.Predef.Int] = El()
```

#### Non-empty Lists

You can create a non-empty list by prepending to another list, similar to `scala.List`:

```scala
scala> val x = 1 :: List.empty[Int]
x: dogs.Nel[dogs.Predef.Int] = List(1)
```

Note that the type returned here is `dogs.Nel`. This is different than
what happens with the standard library `::`, which returns `List`:

```scala
scala> val x = 1 :: scala.Nil
x: List[Int] = List(1)
```


### Pattern matching

The pattern `El()` will match empty lists, The parentheses are
required here, for a similar reason that they are required on
[Option](option)'s `None()` pattern.

`Nel(head, tail)` will match on the head and tail of a non-empty list.

```scala
scala> import List._
import List._

scala> def listPrint[A](l: List[A]) = l match {
     |   case El() => println("empty")
     |   case Nel(h,t) => println(s"Non-empty, with $h at head")
     | }
listPrint: [A](l: dogs.List[A])Unit

scala> listPrint(empty)
empty

scala> listPrint(1 :: empty)
Non-empty, with 1 at head
```

### Interoperating with the standard library

There is a method for converting a list to a `scala.collection.immutable.List`

```scala
scala> val x: dogs.List[Int] = List(1,2,3,4,5)
x: dogs.List[dogs.Predef.Int] = List(1, 2, 3, 4, 5)

scala> val y = x.toScalaList
y: List[dogs.Predef.Int] = List(1, 2, 3, 4, 5)
```

There is also a convenience method to create a list from anything implementing the Iterable interface:

```scala
scala> val x = List.fromIterable(scala.collection.immutable.Vector(1,2,3))
x: dogs.List[Int] = List(1, 2, 3)

scala> val y = List.fromIterable(scala.Range(1, 10))
y: dogs.List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

scala> val z = List.fromIterable(scala.collection.immutable.List(1,2,3))
z: dogs.List[Int] = List(1, 2, 3)
```

### Methods on List

Many of the expected methods on `List` exist. We are still missing
many other useful methods, please feel free to contribute!

```scala
scala> import cats.std.int._
import cats.std.int._

scala> import cats.Eval
import cats.Eval

scala> val x: List[Int] = List(1,2,3)
x: dogs.List[dogs.Predef.Int] = List(1, 2, 3)

scala> x.map(_.toString)
res3: dogs.List[String] = List(1, 2, 3)

scala> x.flatMap(x => List(x, x+1))
res4: dogs.List[Int] = List(1, 2, 2, 3, 3, 4)

scala> x.filter(_ % 2 == 0)
res5: dogs.List[dogs.Predef.Int] = List(2)

scala> x.foldLeft(0)(_ + _)
res6: Int = 6

scala> x.foldRight(Eval.now(scala.Vector.empty[Int]))((a,la) => la.map(a +: _))
res7: cats.Eval[scala.collection.immutable.Vector[dogs.Predef.Int]] = cats.Eval$$anon$7@5f54e68f

scala> x.foreach(println)
1
2
3
res8: dogs.Predef.Unit = ()

scala> x.headOption
res9: dogs.Option[dogs.Predef.Int] = Some(1)

scala> x ++ List(6,7,8)
res10: dogs.List[dogs.Predef.Int] = List(1, 2, 3, 6, 7, 8)

scala> x.find(_ == 7)
res11: dogs.Option[dogs.Predef.Int] = None

scala> x.find(_ % 3 == 1)
res12: dogs.Option[dogs.Predef.Int] = Some(1)

scala> x.contains(2)
res13: dogs.Predef.Boolean = true

scala> x.forall(_ < 10)
res14: dogs.Predef.Boolean = true

scala> x.exists(_ < 10)
res15: dogs.Predef.Boolean = true

scala> x.reverse
res16: dogs.List[dogs.Predef.Int] = List(3, 2, 1)

scala> x.take(2)
res17: dogs.List[dogs.Predef.Int] = List(1, 2)

scala> x.drop(1)
res18: dogs.List[dogs.Predef.Int] = List(2, 3)

scala> x.isEmpty
res19: dogs.Predef.Boolean = false
```
