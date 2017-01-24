---
layout: default
title:  "Home"
section: "home"
---

Dogs is a library containing data structures which facilitate pure
functional programming in the Scala programming language. Some of
these are replacements for structures already present in the Scala
standard library, but with improvements in safety, and sanity, some
are data structures for which there is no analogue in the Scala
standard library.

### EXPERIMENTAL

This project is in its infancy, it should be considered beta

### DATA STRUCTURES

Here is a list of the current structures present in dogs:

- [Dequeue](tut/dequeue)   - A double-ended queue
- [Diet](tut/diet)      - Efficient storage of ragnes of values
- [Heap](tut/binaryheap)    - Purelely Functional Binary Heap
- DList     - A list that supports constant-time append
- [Enum](tut/enum)      - An enumeration of values
- ISet      - An intensional Set
- [List](tut/list)      - A singly linked List
- Map       - A tree based Map
- [Option](tut/option)    - A value which may or may not be present
- [Range](tut/range)     - A range of values from an Enum
- [Set](tut/set)       - A tree based, ordered, extensional set
- Streaming - A stream of values with a lazily evaluated tail

## Predef

dogs offers an alternate Predef 

## Testing

In some circumstances, we want to compare two List[A] without converting
the list to an scala.List. We have defined a Matcher to be use along
the ScalaTest matchers. 

- [ListMatcher](tut/listMatcher) - A ScalaTest Matcher for List[A] 
comparison
