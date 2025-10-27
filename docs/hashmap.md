# HashMap

`HashMap` is an **immutable**, high-performance map implementation in `cats.collections`. 


It uses a hashing system from `cats.kernel.Hash` to keep track of its keys. Under the hood, it relies on a clever data structure called **CHAMP**. This setup helps it deliver fast lookups, efficient updates, and minimal memory overhead, all while preserving immutability.

## How CHAMP Powers HashMap

**CHAMP** (Compressed Hash-Array Mapped Prefix-tree) is a modern trie-based design optimized for immutable hash tables. CHAMP uses:

- **Bitmap-compressed nodes** to track occupied slots, reducing memory waste  
- **5-bit hash chunks** to navigate a 32-ary trie (log₃₂ depth) which keeps the tree shallow for fast lookups and updates 
- **Structural sharing** to reuse unchanged subtrees during updates, saving memory and time  
- **Cache-friendly layouts** store data close together, making access faster



## Usage
`HashMap[K, V]` stores key–value pairs:  
- **K** = key (e.g., a name, ID, or lookup label)  
- **V** = value (e.g., a score, setting, or associated data)

### a. Create an empty HashMap

```scala mdoc
import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

// Create an empty HashMap
val emptyScores = HashMap.empty[String, Int]
println(emptyScores)                    //HashMap()

```
### b. Add entries
```scala mdoc
val scores = HashMap("Alice" -> 95, "Bob" -> 88)
println(scores.size)                    // 2
println(emptyScores ++ scores)          //HashMap(Bob -> 88, Alice -> 95)           
```

### c. Update value
```scala mdoc
val updateBobScore = scores.updated("Bob", 70)
println(updateBobScore.get("Bob"))      // Some(70)
println(updateBobScore)                 //HashMap(Alice -> 95, Bob -> 70)              
```

### d. Remove an entry
```scala mdoc
val withoutBob = scores - "Bob"
println(withoutBob.size)                // 1
println(withoutBob.contains("Bob"))     //false
```

Every operation on an immutable HashMap creates a new instance — the original map (scores) is never changed.

## Performance Characteristics

- Fast operations: Lookups, inserts, updates, and deletes are all very quick.

- Predictable speed: Performance stays consistent as your data grows.

- Low memory use: Only stores what’s needed and shares unchanged parts when you make a new version.



## When to Use HashMap

Prefer `HashMap` over Scala’s standard immutable `Map` when you:

- Work in a **purely functional** codebase (e.g., with Cats, ZIO, or fs2)  
- Need **frequent updates** without sacrificing performance  
- Value **predictable memory usage** and **thread safety** via immutability  
- Build interpreters, caches, or stateful pipelines that rely on persistent data  


## References

- Steindorfer, M. J. (2019).  
   **[Efficient Immutable Collections](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf)**.  
   PhD Thesis, Vrije Universiteit Amsterdam.

- **Cats Collections** – https://typelevel.org/cats-collections/

- **ptimizing Hash-Array Mapped Tries for
Fast and Lean Immutable JVM Collections** - https://michael.steindorfer.name/publications/oopsla15.pdf?utm_source
