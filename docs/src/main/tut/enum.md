---
layout: default
title:  "Enum"
source: "core/src/main/scala/Enum.scala"
---
# Enum

`Enum` represents discrete operations that can be performed on a type `A`

These operations are presented by the following functions.

- `def succ(x)`:		Returns the successor value of `x`
- `def pred(x)`: 	Returns the predecessor value of `x`
- `def adj(i, j)`:	Returns if `i` and `j` are consecutives ( `succ(i) is j`)

An example of the discrete operation on interger values could be: 

```tut
import dogs._, Predef._, dogs.Order._

implicit val intEnum: Enum[Int] = new Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = if (l < r) LT else if (l > r) GT else EQ
  }
```