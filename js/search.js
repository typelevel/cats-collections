// When the user clicks on the search box, we want to toggle the search dropdown
function displayToggleSearch(e) {
  e.preventDefault();
  e.stopPropagation();

  closeDropdownSearch(e);
  
  if (idx === null) {
    console.log("Building search index...");
    prepareIdxAndDocMap();
    console.log("Search index built.");
  }
  const dropdown = document.querySelector("#search-dropdown-content");
  if (dropdown) {
    if (!dropdown.classList.contains("show")) {
      dropdown.classList.add("show");
    }
    document.addEventListener("click", closeDropdownSearch);
    document.addEventListener("keydown", searchOnKeyDown);
    document.addEventListener("keyup", searchOnKeyUp);
  }
}

//We want to prepare the index only after clicking the search bar
var idx = null
const docMap = new Map()

function prepareIdxAndDocMap() {
  const docs = [  
    {
      "title": "Binary Heap",
      "url": "/cats-collections//binaryheap.html",
      "content": "Binary Heap Heap is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based on Vladimir Kostyukov’s paper and it does support the basic operations on a heap without compromising performance. Supported Operations add: add a value to the heap remove: remove a value from the heap (it removes the min value) (--) operator: remove a value from the heap (it removes the min value) min: min value in the heap (it does not remove it) toList: list of sorted values in the heap Example usage: Start by creating an empty Heap: import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._ val h = Heap.empty[Int] // h: Heap[Int] = Leaf h.isEmpty // res0: Boolean = true h.show // res1: String = \"Heap()\" Asking for the min value of a empty heap h.getMin // res2: Option[Int] = None And we can add an item: val h2 = h.add(12) // h2: Heap[Int] = Branch(12, Leaf, Leaf) h2.isEmpty // res3: Boolean = false h2.show // res4: String = \"Heap(12)\" Let’s add a new items and ask for the min value: val h3 = h2 + 5 + 1 + 7 // h3: Heap[Int] = Branch( // 1, // Branch(7, Branch(12, Leaf, Leaf), Leaf), // Branch(5, Leaf, Leaf) // ) h3.getMin // res5: Option[Int] = Some(1) h3.show // res6: String = \"Heap(1, 5, 7, 12)\" If we call remove it removes the min value: val r = h3.remove // r: Heap[Int] = Branch(5, Branch(7, Leaf, Leaf), Branch(12, Leaf, Leaf)) r.show // res7: String = \"Heap(5, 7, 12)\" Heap sub projects More complex implementation of Heap will be added as sub projects where each implementation can be used for specific requirements"
    } ,    
    {
      "title": "Dequeue",
      "url": "/cats-collections//dequeue.html",
      "content": "Dequeue Dequeue is a double-ended queue. It offers constant time cons and snoc, and amortized constant time uncons and unsnoc. Let’s define these terms first: cons: to push a value onto the left of the list. snoc: to push a value onto the right of the list. uncons: to pop a value off the left of the list. unsnoc: to pop a value off the right of the list. It is also worth spending some time discussing what we mean by “amortized constant time”. Internally, the Dequeue uses Lists as stacks to store items which are pushed onto either side. Lets say we currently have a Dequeue containing the numbers 1 through 8, this might be stored like this: left stack: right stack: 1 8 2 7 3 6 4 5 In this configuration, we can pop off three items from the left side in constant time (O(1)). However, we will have to do more computation when we pop off the 4th. Since we don’t want to leave either stack completely empty, when we pop the 4th item off the left, we will grab half of the elements from the bottom of the right stack, to make more available in the left stack. So before this operation, we have: left stack: right stack: (empty) 8 7 6 5 And after, we have: left stack: right stack: 5 8 6 7 This process of reshuffling the stacks to make sure we have items available on both sides is actually a linear (O(n)) operation. However, since we only have to perform this operation after every n/2 pop operations, we can say that this is constant time when amortized over enough operations."
    } ,    
    {
      "title": "Diet",
      "url": "/cats-collections//diet.html",
      "content": "Diet Diet is a Discrete Interval Encoding Tree. It stores subset of types that have a total order. The types are also required to have a successor and predecessor generator. The discrete interval encoding tree is based on the observation that the set of integers { i | a&lt;=i&lt;=b } can be perfectly represented by the closed interval [a, b]. Diet is a binary search tree where each node contains a set of continuous values. If a value is inserted and it fills the hole between to nodes (sets) then the two set become one since all value in the new set are consecutive. Best and Worst Case Analysis. The best case is when there is no holes in the stored set, so the tree contains only one node (a single interval). In this case, it has O(1) space requirement, inserting, deleting and finding a value is in constant time. In the worst case scenario, there is a hole of size one (1) between each node and each node is in fact a set on size one (1). Under these circumstances, operations in the tree required O(n). Supported Operations add: add a value to the tree addRange: add the entire range to the tree remove: remove a value from the tree removeRange: remove a range from the tree contains: verify is a value is on the tree containsRange: verify that an interval is on the tree (-) operator: remove a range from the tree (&amp;) operator: calculates the intersection with another Diet (|) or (++) operator: calculates the union with another Diet min: min value in the tree max: max value in the tree intervals: the list of all intervals (sets) in the tree. The sets are disjoint sets. toList: list of sorted values in the tree Diet is showable so we can call show on it. Example usage: Start by creating an empty Diet: import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._ val d: Diet[Int] = Diet.empty // d: Diet[Int] = EmptyDiet d.isEmpty // res0: Boolean = true d.show // res1: String = \"Diet( )\" And we can add an item: val d2 = d.add(12) // d2: Diet[Int] = DietNode(Range(12, 12), EmptyDiet, EmptyDiet) d2.isEmpty // res2: Boolean = false d2.show // res3: String = \"Diet( [12, 12] )\" And now we can check that it thinks 12 is in the set, but not other numbers d2.contains(1) // res4: Boolean = false d2.contains(12) // res5: Boolean = true If we remove our only element, we get back to the empty Diet: val d3 = d2.remove(12) // d3: Diet[Int] = EmptyDiet d3.isEmpty // res6: Boolean = true Asking to remove an element not in the set is a noop: val s = Diet.empty[Int].remove(10) // s: Diet[Int] = EmptyDiet s.show // res7: String = \"Diet( )\" Diet excels at storing ranges, so there are also operations that work on ranges of values: val d = Diet.empty[Int].addRange(1 toIncl 20) // d: Diet[Int] = DietNode(Range(1, 20), EmptyDiet, EmptyDiet) d.contains(21) // res8: Boolean = false d.contains(20) // res9: Boolean = true d.contains(10) // res10: Boolean = true val d2 = d - (10 toIncl 12) // d2: Diet[Int] = DietNode( // Range(1, 9), // EmptyDiet, // DietNode(Range(13, 20), EmptyDiet, EmptyDiet) // ) d2.show // res11: String = \"Diet( [1, 9] [13, 20] )\" d2.contains(10) // res12: Boolean = false d2.containsRange(1 toIncl 5) // res13: Boolean = true d2.containsRange(11 toIncl 15) // fails since not the entire range is contained // res14: Boolean = false Given two Diets, we can find the union or the intersection: val d1 = Diet.empty[Int] + (5 toIncl 10) // d1: Diet[Int] = DietNode(Range(5, 10), EmptyDiet, EmptyDiet) val d2 = Diet.empty[Int] + (7 toIncl 12) // d2: Diet[Int] = DietNode(Range(7, 12), EmptyDiet, EmptyDiet) (d1 &amp; d2).show // res15: String = \"Diet( [7, 10] )\" (d1 | d2).show // res16: String = \"Diet( [5, 12] )\" Asking to remove non existing range yields the same diet val d = Diet.empty[Int].addRange((5 toIncl 20)) // d: Diet[Int] = DietNode(Range(5, 20), EmptyDiet, EmptyDiet) val d1 = d.removeRange((1 toIncl 4)) // d1: Diet[Int] = DietNode(Range(5, 20), EmptyDiet, EmptyDiet) d1.show // res17: String = \"Diet( [5, 20] )\" Asking to remove a range yields a new Diet without the range val d = Diet.empty[Int].addRange((5 toIncl 20)).addRange(22 toIncl 30) // d: Diet[Int] = DietNode( // Range(5, 20), // EmptyDiet, // DietNode(Range(22, 30), EmptyDiet, EmptyDiet) // ) val d2 = d.removeRange((5 toIncl 20)) // d2: Diet[Int] = DietNode(Range(22, 30), EmptyDiet, EmptyDiet) d2.show // res18: String = \"Diet( [22, 30] )\" Asking to remove a sub-range splits the Diet val d = Diet.empty[Int].addRange((5 toIncl 20)) // d: Diet[Int] = DietNode(Range(5, 20), EmptyDiet, EmptyDiet) val d3 = d.removeRange((10 toIncl 15)) // d3: Diet[Int] = DietNode( // Range(5, 9), // EmptyDiet, // DietNode(Range(16, 20), EmptyDiet, EmptyDiet) // ) (10 toIncl 15).toList.forall { i =&gt; d3.contains(i) } // res19: Boolean = false (5 toIncl 9).toList.forall {i =&gt; d3.contains(i) } // res20: Boolean = true (16 toIncl 20).toList.forall {i =&gt; d3.contains(i) } // res21: Boolean = true Adding a inverted range val d = Diet.empty[Int] + Range(20, 10) // d: Diet[Int] = DietNode(Range(10, 20), EmptyDiet, EmptyDiet) d.show // res22: String = \"Diet( [10, 20] )\""
    } ,    
    {
      "title": "Discrete",
      "url": "/cats-collections//discrete.html",
      "content": "Discrete Discrete represents discrete operations that can be performed on a type A These operations are presented by the following functions. def succ(x): Returns the successor value of x def pred(x): Returns the predecessor value of x def adj(i, j): Returns if i and j are consecutive ( succ(i) is j) An example of the discrete operation on integer values could be: import cats._, cats.collections._ implicit val intDiscrete: Discrete[Int] = new Discrete[Int] { override def succ(x: Int): Int = x + 1 override def pred(x: Int): Int = x - 1 } // intDiscrete: Discrete[Int] = repl.MdocSession$App$$anon$1@268608ae"
    } ,    
    {
      "title": "DisjointSets",
      "url": "/cats-collections//disjointsets.html",
      "content": "Disjoint Sets DisjointSets provides a purely functional implementation for the union-find collection. An Union-Find (aka Disjoint Sets) structure is a set of sets where the intersection of any two sets is empty. This constraint opens the way to fast unions (O(1) average). That makes of Disjoint Sets the perfect tool for clustering algorithms such as calculating the connected components in a graph. Initially, it is a flat collection where each element forms its own, size 1, disjoint set. New elements are added as new disjoint sets and union operations can be used to fusion these sets. The joined sets are represented by one of its elements known as label or root. Fast fusion of disjoints sets is key and its provided through parenthood relations. Sets labels are always the top-level ancestor. The following example shows 3 disjoint sets of size 1. Whereas, in the next one, C is the parent of the {A, B, C} set which is the only set in the DisjointSets structure. Supported operations dsets + c (add): Adds a value as a single element set: desets.union(A,C) (union/join): Fusion two disjoint sets: dsets.find(v) (find): Retrieves Some(label) if v belongs to the set with that label or None if the value is not part of dsets. dsets.toSets: Gets a Map[T, Set[T]] representation of the DisjointSets contents where the key is each set label. Example usage import DisjointSets._ val operations = for { _ &lt;- union(1,2) oneAndTwo &lt;- find(2) _ &lt;- union(3,4) threeAndFour &lt;- find(3) _ &lt;- union(2,3) sets &lt;- toSets } yield sets val label2disjointset: Map[Int, Set[Int]] = operations.runA(DisjointSets(1,2,3,4)).value Structure and performance The main idea is that each element starts as a disjoint set itself. A set with two or more elements is always the result of one or several union operations. Thus, a multi-element set is composed of sets of just one element, call them components. Each component has 3 fields: One for the value of the element it contains. A reference pointing to another component of the same composed multi-element set. Or itself if it constitutes a single element set. An estimation of how many components/nodes compose its descendants. This estimation is known as rank. Let’s assume that the next operations are executed: dsets.union(B,A) //1 dsets.union(B,C) //2 From a mathematical point of view, the result should be similar to the one shown below: However, in order to improve lookups performance, some optimizations need to be applied. Therefore, with optimized operations, the resulting structure after (1) and (2) is: Each set is a tree represented by its root. Therefore, looking for the set an element belongs to is not more than following its parental relations until the root of the tree is reached. So the shallower the tree is, the fewer the operations to be performed in the lookup are. On the other hand, the operation where two sets are merged (union) consist on making one of the two trees to become a branch of the other: Heuristics These are two optimizations whose goal is to reduce the depth of each tree representing a set. The lower the tree height is, the fewer operations are needed to find one element set label. Considering DisjointSets structure as a forest of tree-sets, let’s compute as the maximum tree height of the forest. The maximum number of operations to complete before finding one element in one set is proportional to the number of jumps from the deepest leaf to its tree root. That is . There are two heuristics designed to keep these tress low: Union by rank: Avoids incrementing whenever possible. The idea is for each value node to store how many levels have the sub-tree that they are ancestor of (call it rank). All new elements have a rank value of 0 (the root is the first level). This way, whenever an union operation is executed, is the shorter tree the one that becomes a branch of the other and, therefore, does not increase. Path compression: In this case the aim is to take advantage of the jumps made in find operation. Whenever a tree root is found it is assigned to every node in the search path. This is a purely functional data structure so the changes are reflected in a new copy of it which forms part of the operation result."
    } ,    
    {
      "title": "Home",
      "url": "/cats-collections//extra_md/",
      "content": "Cats-Collections It intends to be a library containing data structures which facilitate pure functional programming in the Scala programming language. Some of these are replacements for structures already present in the Scala standard library, but with improvements in safety, some are data structures for which there is no analogue in the Scala standard library. Getting Started Cats Collections is currently available for Scala 2.11, 2.12 and 2.13. To get started with sbt, simply add the following to your build.sbt file: libraryDependencies += \"org.typelevel\" %% \"cats-collections-core\" % \"0.9.0\" CONTRIBUTING Contributions in all forms (including discussion, suggestions, bug reports, usage reports, pull requests, patches, etc) are welcome and encouraged from everyone! The cats-collection project demands that all activities surrounding the project follow the Typelevel Code of Conduct. If you witness any behavior from any individual which you feel might be harmful in any way or might not be a violation of the code of conduct, please speak up. Please contact any individual named in the Typelevel Code of Conduct so that it can be addressed as soon as possible."
    } ,        
    {
      "title": "Predicate",
      "url": "/cats-collections//predicate.html",
      "content": "Predicate Predicate is a function that returns a Boolean, depending on whether the argument satisfies the predicate condition."
    } ,    
    {
      "title": "Range",
      "url": "/cats-collections//range.html",
      "content": "Range Range represents a range [x, y] that can be generated by using discrete operations Discrete. Supported Operations start: starting value of Range. end: ending value of Range. toList: returns all the values in the Range as a List. contains(value): verifies value is within the Range. contains(range): verifies range is within the Range. reverse: returns the reverted range Range [end, start] -(other): Calculate the difference with Range. It returns a tuple with the difference to the right and to the left of Range. It basically calculates what is to the left of other that is in Range and what is to the right of other that is in Range (in both cases it does not include elements in other) toString: returns Inverted Range Note that if x &gt; y and we create the range [x, y] it will be treated as the inverted range [y, x]. Using Range We can get the values from a range using generate or toList functions. We can also get the string representing the range in math notation. Range(x, y) is represented by [x, y] and Range.empty is represented by [] by using cats.Show import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._, cats.collections.Range._ val range = Range(1, 10) // range: Range[Int] = Range(1, 10) range.show // res0: String = \"[1, 10]\" range.toList // res1: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) We can get the inverted range using the reverse functions range.reverse.show // res2: String = \"[10, 1]\" Asking for start and end on a Range val range = Range(1, 10) // range: Range[Int] = Range(1, 10) range.start // res3: Int = 1 range.end // res4: Int = 10 Asking for a value within Range val range = Range(1, 10) // range: Range[Int] = Range(1, 10) range.contains(5) // res5: Boolean = true Asking for a value that is not within Range val range = Range(1, 10) // range: Range[Int] = Range(1, 10) range.contains(20) // res6: Boolean = false Asking for the difference between two Ranges returns 0, 1, or 2 result ranges val range = Range(1, 10) // range: Range[Int] = Range(1, 10) Range(10, 20).show // res7: String = \"[10, 20]\" (range - Range(5, 9)).show // res8: String = \"Some(([1, 4],Some([10, 10])))\" (range - Range(30, 40)).show // res9: String = \"Some(([1, 10],None))\" (range - Range(15, 18)).show // res10: String = \"Some(([1, 10],None))\" (range - Range (5, 30)).show // res11: String = \"Some(([1, 4],None))\" Creating an inverted range val range = Range(50, 20) // range: Range[Int] = Range(50, 20) range.toList // res12: List[Int] = List( // 50, // 49, // 48, // 47, // 46, // 45, // 44, // 43, // 42, // 41, // 40, // 39, // 38, // 37, // 36, // 35, // 34, // 33, // 32, // 31, // 30, // 29, // 28, // 27, // 26, // 25, // 24, // 23, // 22, // 21, // 20 // ) The reverse of a range should be its inverted range val range = Range(20, 30) // range: Range[Int] = Range(20, 30) range.show // res13: String = \"[20, 30]\" val other = Range(30, 20) // other: Range[Int] = Range(30, 20) other.show // res14: String = \"[30, 20]\" range.reverse.toList == other.toList // res15: Boolean = true"
    } ,      
    {
      "title": "AvlSet",
      "url": "/cats-collections//set.html",
      "content": "AvlSet AvlSet is a tree-based set which stores Orderable elements in a AVL balanced binary tree. This set is an Extensional Set, which is to say that membership of the set is defined by enumerating the members."
    }    
  ];

  idx = lunr(function () {
    this.ref("title");
    this.field("content");

    docs.forEach(function (doc) {
      this.add(doc);
    }, this);
  });

  docs.forEach(function (doc) {
    docMap.set(doc.title, doc.url);
  });
}

// The onkeypress handler for search functionality
function searchOnKeyDown(e) {
  const keyCode = e.keyCode;
  const parent = e.target.parentElement;
  const isSearchBar = e.target.id === "search-bar";
  const isSearchResult = parent ? parent.id.startsWith("result-") : false;
  const isSearchBarOrResult = isSearchBar || isSearchResult;

  if (keyCode === 40 && isSearchBarOrResult) {
    // On 'down', try to navigate down the search results
    e.preventDefault();
    e.stopPropagation();
    selectDown(e);
  } else if (keyCode === 38 && isSearchBarOrResult) {
    // On 'up', try to navigate up the search results
    e.preventDefault();
    e.stopPropagation();
    selectUp(e);
  } else if (keyCode === 27 && isSearchBarOrResult) {
    // On 'ESC', close the search dropdown
    e.preventDefault();
    e.stopPropagation();
    closeDropdownSearch(e);
  }
}

// Search is only done on key-up so that the search terms are properly propagated
function searchOnKeyUp(e) {
  // Filter out up, down, esc keys
  const keyCode = e.keyCode;
  const cannotBe = [40, 38, 27];
  const isSearchBar = e.target.id === "search-bar";
  const keyIsNotWrong = !cannotBe.includes(keyCode);
  if (isSearchBar && keyIsNotWrong) {
    // Try to run a search
    runSearch(e);
  }
}

// Move the cursor up the search list
function selectUp(e) {
  if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index) && (index > 0)) {
      const nextIndexStr = "result-" + (index - 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Move the cursor down the search list
function selectDown(e) {
  if (e.target.id === "search-bar") {
    const firstResult = document.querySelector("li[id$='result-0']");
    if (firstResult) {
      firstResult.firstChild.focus();
    }
  } else if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index)) {
      const nextIndexStr = "result-" + (index + 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Search for whatever the user has typed so far
function runSearch(e) {
  if (e.target.value === "") {
    // On empty string, remove all search results
    // Otherwise this may show all results as everything is a "match"
    applySearchResults([]);
  } else {
    const tokens = e.target.value.split(" ");
    const moddedTokens = tokens.map(function (token) {
      // "*" + token + "*"
      return token;
    })
    const searchTerm = moddedTokens.join(" ");
    const searchResults = idx.search(searchTerm);
    const mapResults = searchResults.map(function (result) {
      const resultUrl = docMap.get(result.ref);
      return { name: result.ref, url: resultUrl };
    })

    applySearchResults(mapResults);
  }

}

// After a search, modify the search dropdown to contain the search results
function applySearchResults(results) {
  const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    //Remove each child
    while (dropdown.firstChild) {
      dropdown.removeChild(dropdown.firstChild);
    }

    //Add each result as an element in the list
    results.forEach(function (result, i) {
      const elem = document.createElement("li");
      elem.setAttribute("class", "dropdown-item");
      elem.setAttribute("id", "result-" + i);

      const elemLink = document.createElement("a");
      elemLink.setAttribute("title", result.name);
      elemLink.setAttribute("href", result.url);
      elemLink.setAttribute("class", "dropdown-item-link");

      const elemLinkText = document.createElement("span");
      elemLinkText.setAttribute("class", "dropdown-item-link-text");
      elemLinkText.innerHTML = result.name;

      elemLink.appendChild(elemLinkText);
      elem.appendChild(elemLink);
      dropdown.appendChild(elem);
    });
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdownSearch(e) {
  // Check if where we're clicking is the search dropdown
  if (e.target.id !== "search-bar") {
    const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
    if (dropdown) {
      dropdown.classList.remove("show");
      document.documentElement.removeEventListener("click", closeDropdownSearch);
    }
  }
}
