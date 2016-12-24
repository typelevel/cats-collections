---
layout: default
title:  "Dequeue"
source: "core/src/main/scala/Dequeue.scala"
---
# Dequeue

`Dequeue` is a double-ended queue. It offers constant time cons and
snoc, and amortized constant time uncons and unsnoc. Let's define
these terms first:

- cons: to push a value onto the left of the list.
- snoc: to push a value onto the right of the list.
- uncons: to pop a value off the left of the list.
- unsnoc: to pop a value off the right of the list.

It is also worth spending some time discussing what we mean by
"amortized constant time". Internally, the `Dequeue` uses Lists as
stacks to store items which are pushed onto either side. Lets say we
currently have a Dequeue containing the numbers 1 through 8, this
might be stored like this:

     left stack:                     right stack:
          1                               8
          2                               7
          3                               6
          4                               5
		  
In this configuration, we can pop off three items from the right side
in constant time (O(1)). However, we will have to do more computation
when we pop off the 4th. Since we don't want to leave either stack
completely empty, when we pop the 4th item off the left, we will grab
half of the elements from the bottom of the right stack, to make more
available in the left stack. So before this operation, we have:

     left stack:                     right stack:
       (empty)                            8
                                          7
                                          6
                                          5

And after, we have:

     left stack:                     right stack:
         5                                8
         6                                7

This process of reshuffling the stacks to make sure we have items
available on both sides is actually a linear (O(n))
operation. However, since we only have to perform this operation after
every n pop operations, we can say that this is constant time when
amortized over enough operations.
