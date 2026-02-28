# BitSet

`BitSet` is a purely functional, immutable bitset that efficiently
stores non-negative integer values. It is implemented as a tree of
fixed-size arrays, providing good performance for both sparse and
dense sets of integers.

## Supported Operations

- `+` / `-`:          add or remove a single value
- `|`:                union of two bitsets
- `&`:                intersection of two bitsets
- `^`:                exclusive-or (symmetric difference)
- `--`:               difference (left minus right)
- `intersects`:       check whether two bitsets share any values
- `compact`:          remove empty internal branches to save memory
- `apply(n)`:         check whether a value is present
- `size`:             number of distinct values
- `isEmpty`:          check whether the bitset is empty
- `iterator`:         iterate values in unsigned order
- `toSet`:            convert to a `scala.Set[Int]`

## Example Usage

Create a `BitSet` from individual values:

```scala mdoc
import cats.collections._

val empty = BitSet.empty

empty.isEmpty

val set = BitSet(1, 2, 3, 5, 8)

set.size
```

Check membership and add/remove values:

```scala mdoc
set(3)

set(4)

val withFour = set + 4

withFour(4)

val withoutThree = set - 3

withoutThree(3)
```

Set operations — union, intersection, difference, and xor:

```scala mdoc
val evens = BitSet(2, 4, 6, 8, 10)
val odds = BitSet(1, 3, 5, 7, 9)

val union = evens | odds
union.iterator.toList

val small = BitSet(1, 2, 3, 4, 5)

val intersection = evens & small
intersection.iterator.toList

val diff = small -- evens
diff.iterator.toList
```

Check whether two bitsets overlap:

```scala mdoc
evens.intersects(odds)

evens.intersects(small)
```

Build a `BitSet` from a Scala `Range`:

```scala mdoc
val range = BitSet.fromScalaRange(1 to 10)

range.size

range.iterator.toList
```

Convert to a standard Scala `Set`:

```scala mdoc
val scalaSet = set.toSet

scalaSet
```
