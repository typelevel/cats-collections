---
layout: page
title:  "List"
source: "core/src/main/scala/Option.scala"
------------------------------------------

# ListMatcher

`ListMatcher` is a `ScalaTest Matcher` to be *mix* within our test classes
  
## Supported Operations
 
- matchTo:  Match the `dogs.List[A]` to another `dogs.List[A]`. Note
that we need to know how to compare elements of type `A`. This is
described by `Order[A]`.


## Using Range

In order to use this new matcher, we only need to mix it in our test
classes. An example would be the `DietTest` class.

```
class DietTest extends FlatSpec with Matchers with ListMatcher {
    it should "be always sorted" in {
        val diet = Diet.empty[Int].add(5).add(6).add(1).add(3).add(2).add(8)
    
        val sorted = diet.toList()
    
        sorted should matchTo(List(1, 2, 3, 5, 6, 8))
      }
}
```


