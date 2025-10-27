# HashSet

`HashSet` is an **immutable**, high-performance set implementation in `cats.collections`. 


HashSet uses `cats.kernel.Hash` to hash its elements and is built on the **CHAMP** data structure. 
This gives it fast lookups, efficient updates, and low memory use without any mutation


## How CHAMP Powers HashSet

**CHAMP** (Compressed Hash-Array Mapped Prefix-tree) is a modern trie-based design optimized for immutable hash tables. CHAMP uses:

- **Bitmap-compressed nodes** to track occupied slots, reducing memory waste  
- **5-bit hash chunks** to navigate a 32-ary trie (log₃₂ depth) which keeps the tree shallow for fast lookups and updates 
- **Structural sharing** to reuse unchanged subtrees during updates, saving memory and time  
- **Cache-friendly layouts** store data close together, making access faster



## Usage
`HashSet[A]` holds unique values. No duplicates. No order. Immutable.  


### a. Create an empty set

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

// Create an empty HashMap
val nofruits = HashSet.empty[String]
println(nofruits)

```
### b. Add items
```scala mdoc
val fruits = nofruits + "apple" + "banana"
println(fruits.size)                      // 2
println(fruits)                           //HashSet(banana, apple)                   
```

### c. Check if an item is in the set
```scala mdoc
println(fruits.contains("apple"))         // true
```

### d. Remove an item
```scala mdoc
val withoutApple = fruits - "apple"
println(withoutApple.size)                // 1
println(withoutApple)                     //HashSet(banana)     
```

### e. Set Operations
```scala
val otherFruits = HashSet("cherry", "banana")
val union        = fruits ++ otherFruits       
println(union)                                  // HashSet("apple", "banana", "cherry")
val intersection = fruits & otherFruits         
println(intersection)                           // HashSet("banana")
val difference   = fruits -- otherFruits
println(difference)                             // HashSet("apple")
```

## Performance Characteristics

- Fast membership tests: Checking if an item is in the set takes near-constant time.
- Quick adds and removes: Adding or removing elements is efficient, thanks to structural sharing.
- Low memory footprint: Reuses unchanged parts of the set when you create a new version, no wasted space.
.



## When to Use HashSet

Prefer `HashSet` over Scala’s standard immutable `Set` when you:

- Work in a **purely functional** codebase (e.g., with Cats, ZIO, or fs2)  
- Need **frequent updates** without sacrificing performance  
- Value **predictable memory usage** and **thread safety** via immutability  
- Build interpreters, caches, or stateful pipelines that rely on persistent data  


## References

- Steindorfer, M. J. (2019).  
   **[Efficient Immutable Collections](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf)**.  
   PhD Thesis, Vrije Universiteit Amsterdam.

- **Cats Collections** – https://typelevel.org/cats-collections/

    **HashSet | Piotr Kosmowski** - https://kospiotr.github.io/docs/notes/development/data_structures/hash_set/?utm_source
