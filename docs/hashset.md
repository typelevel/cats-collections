# HashSet

`HashSet` is an immutable hash set implemented with the **CHAMP** (Compressed Hash-Array Mapped Prefix-tree) encoding.  
It stores an unordered collection of unique elements of type `A` and relies on a `cats.kernel.Hash[A]` instance for hashing and typeclass-aware equality.

This Cats Collections implementation is derived from Scala’s immutable `HashSet` and adapted to integrate with Cats typeclasses.

The CHAMP trie splits 32-bit hashes into 5-bit partitions at successive depths, using compact bitmaps and small arrays to represent node contents. When many elements map to the same 5-bit segment (or to the same 32-bit hash), the implementation uses sub-nodes or a collision node to preserve correctness.

---

## Best and Worst Case Analysis

- **Best case (well distributed hashes)**  
  Most operations — lookup (`contains`), insertion (`add`), deletion (`remove`) — run in expected **O(1)** time on average.

- **Worst case (heavy hash collisions or adversarial hashes)**  
  Many elements colliding to the same 32-bit hash lead to a `CollisionNode` collection; operations can degrade toward **O(n)** for the colliding elements. 

Memory usage is efficient for broad distributions due to compact bitmaps and structural sharing; however collision nodes and copying on updates can increase memory footprint in degenerate cases.

---

## Supported Operations

- `empty` — create an empty `HashSet`.
- `apply(as: A*)` / `fromSeq` / `fromIterableOnce` / `fromFoldable` — construct a `HashSet` from collections.
- `iterator` — one-time iterator over elements.
- `size` — number of elements.
- `isEmpty` / `nonEmpty` — emptiness checks.
- `foreach(f)` — iterate for side effects.
- `contains(value)` — membership test.
- `add(value)` / `+` — return a new set with `value` added.
- `remove(value)` / `-` — return a new set with `value` removed.
- `union(set)` / `union(iterable)` — union of sets.
- `diff(set)` / `diff(iterable)` — difference (this \ that).
- `intersect(set)` — set intersection.
- `filter(f)` / `filterNot(f)` — retain / drop elements by predicate.
- `toSet` — convert to a standard Scala `Set` wrapper (`WrappedHashSet`).
- `===(that)` — typesafe equality (uses `Eq` semantics).
- `equals`, `hashCode`, `toString` — standard JVM-style operations.
- `show` (via `Show` instance) — pretty printing using `cats.Show`.
- `improve(hash: Int)` — hash mixing helper (private utility).

---

## `HashSet` is *showable* and integrates with Cats typeclasses

`HashSet` supports and provides instances for several Cats and Cats-Kernel typeclasses:

- `UnorderedFoldable[HashSet]`
- `CommutativeMonoid[HashSet[A]]` (union)
- `Show[HashSet[A]]`
- `Hash[HashSet[A]]`
- `DistributiveLattice[HashSet[A]]` (join = union, meet = intersection)

There are also concrete monoid implementations:
- `HashSetUnionMonoid[A]` — combines by union.
- `HashSetIntersectionMonoid[A]` — combines by intersection.

---

## Internal structure (developer reference — Node API)

### `HashSet.Node[A]` (abstract)

Defines the common API for CHAMP nodes.

- `allElementsCount`, `valueCount`, `nodeCount`, `size`
- `getValue(index)`, `getNode(index)`
- `hasNodes`, `hasValues`
- `foreach(f)`, `contains(element, hash, depth)`, `add`, `remove`, `===`
- `sizeHint`: approximation used for deletions

### `CollisionNode[A]`

Handles multiple elements sharing the same hash.  
Contains `collisionHash`, `contents: NonEmptyVector[A]`, and implements `contains`, `add`, `remove`, `===`.

### `BitMapNode[A]`

Main trie node with `valueMap`, `nodeMap`, and `contents: Array[Any]`.  
Provides value and node indexing, bitmap-based lookup, and efficient merges of subtrees.

---

## Iterator

`HashSet.Iterator[A]` performs depth-first traversal without recursion, using fixed-size arrays for stack and cursor management.  
`hasNext` and `next()` follow standard Scala iterator semantics.

---

## Companion utilities and factories

- `improve(hash: Int)` — hash mixing utility.
- `empty[A]`, `apply[A](as: A*)`, `fromSeq`, `fromIterableOnce`, `fromFoldable` — constructors for creating `HashSet`s.

---

## Typeclass instances provided (complete list)

- `UnorderedFoldable[HashSet]`
- `CommutativeMonoid[HashSet[A]]`
- `Show[HashSet[A]]`
- `Hash[HashSet[A]]`
- `DistributiveLattice[HashSet[A]]`

Monoid variants:
- `HashSetUnionMonoid[A]`
- `HashSetIntersectionMonoid[A]`

---

## Examples

```scala mdoc
import cats.collections._
import cats.implicits._

val s = HashSet.empty[Int]
s.isEmpty
s.size
s.show
```

Add and check:

```scala mdoc
val s1 = s.add(10).add(5).add(20)
s1.contains(10)
s1.show
```

Remove elements:

```scala mdoc
val s2 = s1.remove(10)
s2.show
```

Union / intersection / difference:

```scala mdoc
val a = HashSet(1, 2, 3)
val b = HashSet(3, 4, 5)
(a.union(b)).show
(a.intersect(b)).show
(a.union(b).diff(b)).show
```

Filter:

```scala mdoc
val u = a.union(b)
u.filter(_ % 2 == 0).show
u.filterNot(_ % 2 == 0).show
```

Constructors:

```scala mdoc
HashSet.fromSeq(Seq(1, 2, 3)).show
HashSet.fromIterableOnce(Vector(10, 11, 10)).show
HashSet.fromFoldable(Option(42)).show
```

Iterator and conversion:

```scala mdoc
val it = a.iterator
it.hasNext
it.next()
a.toSet
```

Equality and show:

```scala mdoc
val x = HashSet(1,2,3)
val y = HashSet(3,2,1)
x === y
x.hashCode
x.show
```

---

## Summary

- `HashSet` is an immutable, CHAMP-based set implementation optimized for fast, type-safe hashing.  
- It supports full set algebra and integrates with Cats typeclasses.  
- Efficient, persistent structure with compact memory layout and safe updates.

