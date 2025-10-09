# HashMap

`HashMap` is an immutable hash map using `cats.kernel.Hash` for hashing.

Implemented using the CHAMP encoding, which provides excellent performance characteristics for immutable hash maps.

## CHAMP Encoding

CHAMP (Compressed Hash Array Mapped Prefix-trie) is a highly efficient data structure for immutable hash maps and sets. It offers:

- **Memory efficiency**: Compressed representation using bitmaps
- **Fast lookups**: O(log n) average case with excellent constants
- **Structural sharing**: Efficient updates with minimal copying
- **Cache-friendly**: Good memory locality for performance

## Usage

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

// Create an empty HashMap
val empty = HashMap.empty[String, Int]
empty.isEmpty
```

Add elements:

```scala mdoc
val map1 = empty.updated("key1", 1).updated("key2", 2)
map1.size
```

Update values:

```scala mdoc
val map2 = map1.updated("key1", 10)
map2.get("key1")
```

Remove elements:

```scala mdoc
val map3 = map2.removed("key1")
map3.contains("key1")
```

## Performance Characteristics

- **Lookup**: O(log n) average case
- **Insertion**: O(log n) average case  
- **Deletion**: O(log n) average case
- **Memory**: Compact representation with structural sharing

## When to Use

Prefer `HashMap` over Scala's standard `Map` when:
- You need immutable hash maps with better performance
- Working in functional programming contexts
- Memory efficiency is important
- You want structural sharing benefits

## References

- [Efficient Immutable Collections](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf) - PhD thesis by Michael Steindorfer
