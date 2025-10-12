# HashSet

`HashSet` is an immutable hash set using `cats.kernel.Hash` for hashing.

Implemented using the CHAMP encoding, which provides excellent performance characteristics for immutable hash sets.

## CHAMP Encoding

CHAMP (Compressed Hash Array Mapped Prefix-trie) is a highly efficient data structure for immutable hash maps and sets. It offers:

- **Memory efficiency**: Compressed representation using bitmaps
- **Fast lookups**: O(log n) average case with excellent constants
- **Structural sharing**: Efficient updates with minimal copying
- **Cache-friendly**: Good memory locality for performance

## Usage

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

// Create an empty HashSet
val empty = HashSet.empty[String]
empty.isEmpty
```

Add elements:

```scala mdoc
val set1 = empty.add("element1").add("element2")
set1.size
```

Check membership:

```scala mdoc
set1.contains("element1")
set1.contains("element3")
```

Remove elements:

```scala mdoc
val set2 = set1.remove("element1")
set2.contains("element1")
```

Set operations:

```scala mdoc
val set3 = HashSet("a", "b", "c")
val set4 = HashSet("b", "c", "d")
val union = set3 union set4
val intersection = set3 intersect set4
val difference = set3 diff set4

union.size
intersection.size
difference.size
```

## Performance Characteristics

- **Lookup**: O(log n) average case
- **Insertion**: O(log n) average case  
- **Deletion**: O(log n) average case
- **Memory**: Compact representation with structural sharing

## When to Use

Prefer `HashSet` over Scala's standard `Set` when:
- You need immutable hash sets with better performance
- Working in functional programming contexts
- Memory efficiency is important
- You want structural sharing benefits

## References

- [Efficient Immutable Collections](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf) - PhD thesis by Michael Steindorfer
