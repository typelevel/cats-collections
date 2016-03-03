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

This project is in its infancy, it should be considered pre-alpha, it
might never go anywhere, it could change drastically along the way if
it does go somewhere.

Here is a list of the current structures present in dogs:

- Const     - A value with a phantom type
- [Dequeue](tut/dequeue)   - A double-ended queue
- [Diet](tut/diet)      - Efficient storage of ragnes of values
- DList     - A list that supports constant-time append
- [Enum](tut/enum)      - An enumeration of values
- Eq        - A typeclass for testing equality
- Eval      - An abstraction of possibly lazy values
- Ior       - An inclusive Or: Left, Right, or Both
- ISet      - An intensional Set
- [List](tut/list)      - A singly linked List
- Map       - A tree based Map
- NotNull   - Proof that a value is not Null
- [Option](tut/option)    - A value which may or may not be present
- Order     - A typeclass for values which can be compared
- [Range](tut/range)     - A range of values from an Enum
- [Set](tut/set)       - A tree based, ordered, extensional set
- Show      - A typeclass for turning values into Strings
- Streaming - A stream of values with a lazily evaluated tail
- Validated - An exlusive Or
- Xor       - An exlusive Or

## Predef

dogs offers an alternate Predef 

## Testing

In some circumstances, we want to compare two List[A] without converting
the list to an scala.List. We have defined a Matcher to be use along
the ScalaTest matchers. 

- [ListMatcher](tut/listMatcher) - A ScalaTest Matcher for List[A] 
comparison
