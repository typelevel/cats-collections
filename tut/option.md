---
layout: default
title:  "Option"
source: "core/src/main/scala/Option.scala"
---
# Option

`Option` in dogs is very similar to `Option` in the scala standard library. Some of the notable differences between `Option` in dogs and in the stdlib are:

- There is no `get` method.  This method is not total, if the value is
  None, .get in the stdlib throws an exception.
- There aren't implicit conversions from Option to any other type. In
  the standard library, there are implicit conversions for Option to
  Iterable, which sometimes are convenient, but sometimes unexpected
  and confusing.


## Using Option

### Constructing Options

The preferred method of creating an option is by using the `some` and `none` "smart constructors` on the Option companion object:

```scala
scala> import dogs._, dogs.Predef._, Option._
import dogs._
import dogs.Predef._
import Option._

scala> val x = some(1)
x: dogs.Option[Int] = Some(1)

scala> val y = none[Int]
y: dogs.Option[dogs.Predef.Int] = None
```

Note that in both cases, the type is inferred as Option[Int], and not
as Some[Int] or None. This is intentional, and it is the reason
these are the preferred constructors. If you use the Some and None
constructors directly, the types will be inferred differently:

```scala
scala> val x = Some(1)
x: dogs.Some[Int] = Some(1)

scala> val y = None
y: dogs.None.type = None
```

### Interoperating with scala.Option

There are convenience mathods for translating between `dogs.Option` and `scala.Option`:

```scala
scala> val x = some(1)
x: dogs.Option[Int] = Some(1)

scala> val y = x.toScalaOption
y: Option[Int] = Some(1)

scala> val z = Option.fromScalaOption(y)
z: dogs.Option[Int] = Some(1)
```

### Methods on dogs.Option

Many of the methods you would find on `scala.Option` will also be
found on `dogs.Option`, There is likely some which are missing, please
send a Pull Request!:

```scala
scala> val n = none[Int]
n: dogs.Option[dogs.Predef.Int] = None

scala> val s = some(1)
s: dogs.Option[Int] = Some(1)

scala> n getOrElse 2
res0: Int = 2

scala> s getOrElse 2
res1: Int = 1

scala> n orElse some(2)
res2: dogs.Option[dogs.Predef.Int] = Some(2)

scala> s orElse some(2)
res3: dogs.Option[Int] = Some(1)

scala> n foreach println
res4: dogs.Predef.Unit = ()

scala> s foreach println
1
res5: dogs.Predef.Unit = ()

scala> n map (_ + 1)
res6: dogs.Option[Int] = None

scala> s map (_ + 1)
res7: dogs.Option[Int] = Some(2)

scala> s flatMap (x => Some(x+1))
res8: dogs.Option[Int] = Some(2)

scala> s zip s.map(_ + 1)
res9: dogs.Option[(Int, Int)] = Some((1,2))
```
