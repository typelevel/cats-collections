/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs
package tests

import dogs.Order.Ordering
import dogs.Predef._
import org.scalatest._
import tests.utils._



class BinaryTreeTestAdd extends FlatSpec with Matchers {

   implicit val cmp =  new Order[Int] {
    override def apply(l: Int, r: Int): Ordering = l.compareTo(r) match {
      case 0  => Order.EQ
      case -1 => Order.LT
      case 1  => Order.GT
    }
  }

  "a empty tree" should "be a leaft" in {
    val emptyTree = BinaryTree.empty[Int]

    emptyTree should be(BTNil())
  }

  "add to empty tree" should "return a branch with one item" in {

    val empty = BinaryTree.empty[Int]

    val branch = empty.add(5)

    branch.value should be(Some(5))
  }

  "add to brach to the left" should "insert value into left branch" in {

    val branch = BinaryTree(5)

    val result = branch.add(3)

    result.value should be(Some(5))
    result.left.value should be(Some(3))
  }

  "add to branch to the right" should "insert value into right branch" in {

    val branch = BinaryTree(5)

    val result = branch.add(7)

    result.value should be(Some(5))
    result.right.value should be(Some(7))
  }

  "add" should "balance the tree after item added" in {
    val tree = BinaryTree(3) + 2 + 1

    tree.value should be(Some(2))
    tree.left.value should be(Some(1))
    tree.right.value should be (Some(3))

    val second = tree + 4

    second.right.right.value should be(Some(4))

    val third = second + 5
    third.right.value should be(Some(4))
    third.right.left.value should be(Some(3))
    third.right.right.value should be(Some(5))
  }



}

class BinaryTreeTestJoins extends FlatSpec with Matchers {
  implicit val cmp =  new Order[Int] {
    override def apply(l: Int, r: Int): Ordering = l.compareTo(r) match {
      case 0  => Order.EQ
      case -1 => Order.LT
      case 1  => Order.GT
    }
  }

  "a tree" should "be the same after joined with an empty tree" in {
    val tree = BinaryTree(5) + 3 + 2 + 7 + 9

    val empty = BinaryTree.empty[Int]

    val joined = tree ++ empty

    ScalaListConverter.toScalaList(tree.toLst) should contain inOrderOnly(2, 3, 5, 7, 9)
  }

  it should "the same plus items from the other tree" in {
    val tree = BinaryTree(5) + 3 + 2 + 7 + 9
    val another = BinaryTree(1) + 4 + 8

    val joined = tree ++ another

    ScalaListConverter.toScalaList(joined.toLst) should contain inOrder(1, 2, 3, 4, 5, 7, 8, 9)
  }
}

class BinaryTreeTestWalk extends FlatSpec with Matchers {
  implicit val cmp =  new Order[Int] {
    override def apply(l: Int, r: Int): Ordering = l.compareTo(r) match {
      case 0  => Order.EQ
      case -1 => Order.LT
      case 1  => Order.GT
    }
  }

  "toLst" should "return sorted seq" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val sorted = tree.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(1, 2, 3, 5)
  }

  "walk in in-order" should "return sorted seq" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val sorted = tree.inorder

    sorted should be (tree.toLst)
  }

  "walk in pre-order" should "return node-left-right for each node" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val preorder = tree.preorder

    ScalaListConverter.toScalaList(preorder) should contain inOrder(5, 2, 1, 3)
  }

  "walk in pos-order" should "return left-right-node for each node" in {
    val tree = BinaryTree(5).add(2).add(1).add(3) + 6

    val preorder = tree.posorder

    ScalaListConverter.toScalaList(preorder) should contain inOrder(1, 3, 2, 6, 5)
  }

  "min" should "return most left item" in {
    val tree = BinaryTree(5).add(6).add(7).add(2).add(3).add(1)

    tree.min() should be(Some(1))
  }

  "max" should "return most right item" in {
    val tree = BinaryTree(5).add(6).add(7).add(2).add(3).add(1)

    tree.max() should be(Some(7))
  }

  "min and max" should "return some" in {
    val empty = BinaryTree.empty[Int]

    empty.min() should be(None())
    empty.max() should be(None())
  }
}

class BinaryTreeTestRemove extends FlatSpec with Matchers {
  implicit val cmp = new Order[Int] {
    override def apply(l: Int, r: Int): Ordering = l.compareTo(r) match {
      case 0 => Order.EQ
      case -1 => Order.LT
      case 1 => Order.GT
    }
  }

  "remove" should "return DBNil when empty" in {
    val empty = BinaryTree.empty[Int]

    empty should be(empty.remove(5))
  }

  "remove leaft" should "keep same tree structure - leaft" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val result = tree.remove(1)

    val sorted = result.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(2, 3, 5)
  }

  "remove to the left" should "point parent to child of the one removed (left)" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val result = tree.remove(2)

    val sorted = result.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(1, 3, 5)
  }

  "remove to right" should "point parent to child of of the one removed (right)" in {
    val tree = BinaryTree(5).add(7).add(8).add(2).add(1).add(3)

    val result = tree.remove(7)

    val sorted = result.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(1, 2, 3, 5, 8)
  }
}

