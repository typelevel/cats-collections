---
layout: default
title:  "Option"
source: "core/src/main/scala/Option.scala"
---
# Option

`Option` in dogs is very similar to `Option` in the scala standard library. Some of the notable differences between `Option` in dogs and in the stdlib are:

- There is no `get` method.  This method is not total, if the value is
  None, .get in the stdlib throws an exception.
- `Option` in dogs is invariant instead of covariant.
- There aren't implicit conversions from Option to any other type. In
  the standard library, there are implicit conversions for Option to
  Iterable, which sometimes are convenient, but sometimes unexpected
  and confusing.


## Using Option

### Constructing Options

The preferred method of creating an option is by using the `some` and `none` "smart constructors` on the Option companion object:

```tut
import dogs._, dogs.Predef._, Option._

val x = some(1)
val y = none[Int]
```

Note that in both cases, the type is inferred as Option[Int], and not
as Some[Int] or None. This is intentional, and it is the reason
these are the preferred constructors. If you use the Some and None
constructors directly, the types will be inferred differently:

```tut
val x = Some(1)
val y = None
```

### Interoperating with scala.Option

There are convenience mathods for translating between `dogs.Option` and `scala.Option`:

```tut
val x = some(1)
val y = x.toScalaOption
val z = Option.fromScalaOption(y)
```

### Methods on dogs.Option

Many of the methods you would find on `scala.Option` will also be
found on `dogs.Option`, There is likely some which are missing, please
send a Pull Request!:

```tut
val n = none[Int]
val s = some(1)

n getOrElse 2
s getOrElse 2
n orElse some(2)
s orElse some(2)
n foreach println
s foreach println
n map (_ + 1)
s map (_ + 1)
s flatMap (x => Some(x+1))
s zip s.map(_ + 1)
```
