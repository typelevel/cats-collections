---
layout: docs
title:  "Discrete"
source: "core/src/main/scala/Discrete.scala"
position: 5
---
# Discrete

`Discrete` represents discrete operations that can be performed on a type `A`

These operations are presented by the following functions.

- `def succ(x)`:		Returns the successor value of `x`
- `def pred(x)`: 	Returns the predecessor value of `x`
- `def adj(i, j)`:	Returns if `i` and `j` are consecutive ( `succ(i) is j`)

An example of the discrete operation on integer values could be:

```scala mdoc
import cats._, cats.collections._

implicit val intDiscrete: Discrete[Int] = new Discrete[Int] {
  override def succ(x: Int): Int = x + 1
  override def pred(x: Int): Int = x - 1
}
```
