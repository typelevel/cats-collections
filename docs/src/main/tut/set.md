---
layout: docs
title:  "Set"
source: "core/src/main/scala/Set.scala"
---
# Set

`Set` is a tree-based set which stores [Order](order)able elements in
a [AVL balanced binary tree](https://en.wikipedia.org/wiki/AVL_tree).

This set is an
[Extensional Set](https://en.wikipedia.org/wiki/Extensional_definition),
as opposed to [ISet](iset) which is an
[Intensional Set](https://en.wikipedia.org/wiki/Intensional_definition). Which
is to say that membership of the set is defined by enumerating the
members.
