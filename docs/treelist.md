# TreeList

`TreeList` is a purely functional random-access list based on
Chris Okasaki's *Purely Functional Random Access Lists*. It
provides O(1) prepend and uncons, and O(log N) random access
and update.

A key advantage of `TreeList` over the standard `List` is that
the internal depth is only O(log N), so operations such as
`traverse` and `sequence` are stack-safe even for very large
collections.

## Supported Operations

- `prepend` (`::`):                 add an item to the front: O(1)
- `uncons`:                         remove and return the head and tail: O(1)
- `get(idx)`:                       random access by index: O(log N)
- `updatedOrThis(idx, value)`:      update an element by index: O(log N)
- `size`:                           number of elements: O(log N)
- `++`:                             concatenation: O(left.size)
- `map`:                            transform each element: O(N)
- `flatMap`:                        monadic bind: O(result.size + this.size)
- `toList`:                         convert to a standard `List`: O(N)

## Example Usage

Start by creating a `TreeList`:

```scala mdoc
import cats._, cats.implicits._, cats.collections._

val empty = TreeList.empty[Int]

empty.isEmpty

val list = 1 :: 2 :: 3 :: TreeList.empty[Int]

list.toList

list.show
```

Random access with `get`:

```scala mdoc
list.get(0)

list.get(1)

list.get(5)
```

Prepend and uncons:

```scala mdoc
val list2 = 0 :: list

list2.toList

list2.uncons
```

Update an element by index:

```scala mdoc
val updated = list.updatedOrThis(1, 42)

updated.toList
```

Build from a standard `List`:

```scala mdoc
val fromList = TreeList.fromList(List(1, 2, 3, 4, 5))

fromList.size

fromList.toList
```

Concatenation:

```scala mdoc
val left = TreeList.fromList(List(1, 2, 3))
val right = TreeList.fromList(List(4, 5, 6))

val combined = left ++ right

combined.toList
```
