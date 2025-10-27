# HashMap

`HashMap` is an immutable hash map using [`cats.kernel.Hash`](https://typelevel.org/cats/api/cats/kernel/Hash.html) for hashing.  
It is implemented using the **CHAMP encoding** (Compressed Hash-Array Mapped Prefix Tree).

CHAMP is an efficient persistent data structure that combines **bit-mapped indexing** and **structural sharing**, providing high performance for functional programming.

---

## Internal Representation

The structure of `HashMap` is based on the observation that hash codes can be viewed as **prefix trees** of bits.  
Each key’s hash code is divided into 5-bit segments (for 32 possible branches).  
Each segment determines which path to take down the tree.

For example, a 32-bit hash is broken into up to **7 segments** (since 7 × 5 bits = 35 bits, covering all bits of the hash).

Each node in this tree (a **BitMapNode**) stores:
- A *bitmap* indicating positions of key-value pairs.
- Another *bitmap* indicating positions of child nodes.

When two keys share the same hash, they are stored in a **CollisionNode**, which simply holds all colliding key-value pairs.

This design ensures:
- Efficient updates and lookups (`O(1)` average)
- Memory efficiency through **bitmaps**
- Full immutability and **structural sharing**

---

## Best and Worst Case Analysis

| Case | Description | Space | Time Complexity |
|------|--------------|--------|-----------------|
| **Best Case** | Hashes are uniformly distributed (no collisions). The structure is shallow and branchless. | O(n) | O(1) average for lookup, insert, remove |
| **Worst Case** | All keys share identical hash codes. Stored as a single `CollisionNode`. | O(n) | O(n) per lookup due to linear scan within collisions |

In practice, using a good `Hash` instance (such as `Hash.fromUniversalHashCode`) avoids pathological cases.

---

## Supported Operations

- `empty`: create an empty map  
- `apply`: create a map from key–value pairs  
- `fromSeq`: build from a Scala sequence  
- `fromIterableOnce`: build from any iterable collection  
- `fromFoldable`: build from a Cats Foldable  
- `contains`: test whether a key exists  
- `get`: get the value associated with a key  
- `getOrElse`: get the value or return a default  
- `updated`: add or update a key–value pair  
- `removed`: remove a key  
- `iterator`: iterate over key–value pairs  
- `keysIterator`: iterate over keys  
- `valuesIterator`: iterate over values  
- `===`: type-safe equality check using `Eq`  
- `hash`: compute a hash using `cats.kernel.Hash`  
- `show`: string representation using `cats.Show`

---

## `HashMap` is *showable* and *comparable* so you can call `show` or `===` on it.

---

## Example usage

Start by creating an empty HashMap:

```scala mdoc
import cats._
import cats.implicits._
import cats.collections._

val hm = HashMap.empty[Int, String]
hm.isEmpty
hm.show
```

Add some key-value pairs:

```scala mdoc
val hm2 = hm.updated(1, "One").updated(2, "Two")
hm2.show
```

You can check for existence and get values:

```scala mdoc
hm2.contains(1)
hm2.contains(3)

hm2.get(1)
hm2.getOrElse(3, "Unknown")
```

If we remove an element, we get a new map:

```scala mdoc
val hm3 = hm2.removed(1)
hm3.show
```

Building a map directly:

```scala mdoc
val hm4 = HashMap(1 -> "A", 2 -> "B", 3 -> "C")
hm4.show
```

Creating from a collection:

```scala mdoc:nest
val seqMap = HashMap.fromSeq(Seq(10 -> "X", 20 -> "Y", 30 -> "Z"))
seqMap.contains(20)
seqMap.get(30)
```

Using Cats abstractions:

```scala mdoc:nest
val doubled = seqMap.unorderedTraverse(v => Option(v + v))
doubled.map(_.show)
```

---

## Internal Visualization

Consider inserting keys `1`, `2`, and `33` into an empty `HashMap`.

Their (simplified) hash codes might look like this (in binary):

```
1  => 00001 00000 ...
2  => 00010 00000 ...
33 => 00001 00001 ...
```

- The **first 5 bits** decide the *branch position* at the root level.
- Keys `1` and `33` share the prefix `00001`, so they go into the same branch.
- Within that branch, the next 5 bits are compared:
  - `1` continues at sub-index `00000`
  - `33` continues at sub-index `00001`

The structure becomes:

```
Root
 ├── [00001] → Node
 │     ├── [00000] = (1 -> "One")
 │     └── [00001] = (33 -> "Thirty-Three")
 └── [00010] = (2 -> "Two")
```

If `1` and `33` had identical hashes, they’d be stored together in a `CollisionNode`:
```
CollisionNode(hash=..., values=[(1 -> "One"), (33 -> "Thirty-Three")])
```

This structure allows **fast lookups** by traversing at most one path determined by hash segments.

---

## Example of Equality and Hashing

```scala mdoc
import cats.kernel.instances.int._
import cats.kernel.instances.string._

val a = HashMap(1 -> "A", 2 -> "B")
val b = HashMap(1 -> "A", 2 -> "B")

a === b   // true

a.hash == b.hash // consistent hashing
```

---

## Summary

| Feature | Description |
|----------|--------------|
| **Type** | Immutable HashMap |
| **Hashing** | Uses `cats.kernel.Hash` |
| **Implementation** | CHAMP (Compressed Hash-Array Mapped Prefix Tree) |
| **Handles** | Key collisions using `CollisionNode` |
| **Typeclasses** | `Eq`, `Hash`, `Show`, `UnorderedTraverse`, `CommutativeMonoid` |
| **Complexity** | O(1) average lookup/update |
| **Immutable** | Yes — all updates return a new structure |
