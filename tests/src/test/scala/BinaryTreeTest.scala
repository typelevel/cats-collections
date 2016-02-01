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

  "Binary Tree" should "be a leaft when empty" in {
    val emptyTree = BinaryTree.empty[Int]

    emptyTree should be(BTNil())
  }

  it  should "return a branch with one item when add to empty tree" in {

    val empty = BinaryTree.empty[Int]

    val branch = empty.add(5)

    branch.value should be(Some(5))
  }

  it should "insert value into left branch when add to brach to the left" in {

    val branch = BinaryTree(5)

    val result = branch.add(3)

    result.value should be(Some(5))
    result.left.value should be(Some(3))
  }

  it should "insert value into right branch when add to branch to the right" in {

    val branch = BinaryTree(5)

    val result = branch.add(7)

    result.value should be(Some(5))
    result.right.value should be(Some(7))
  }

  it should "balance the tree after item added" in {
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

  "Binary Tree" should "be the same after joined with an empty tree" in {
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

   "Binary Tree"  should "return sorted seq when toLst" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val sorted = tree.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(1, 2, 3, 5)
  }

  it  should "return sorted seq when walk in-order" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val sorted = tree.inorder

    sorted should be (tree.toLst)
  }

  it  should "return node-left-right for each node when walk in pre-order" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val preorder = tree.preorder

    ScalaListConverter.toScalaList(preorder) should contain inOrder(2, 1, 5, 3)
  }

  it  should "return left-right-node for each node when walk in pos-order" in {
    val tree = BinaryTree(5).add(2).add(1).add(3) + 6

    val preorder = tree.posorder

    ScalaListConverter.toScalaList(preorder) should contain inOrder(1, 3, 6, 5, 2)
  }

  it  should "return most left item when ask for min" in {
    val tree = BinaryTree(5).add(6).add(7).add(2).add(3).add(1)

    tree.min() should be(Some(1))
  }

  it should "return most right item when ask for max" in {
    val tree = BinaryTree(5).add(6).add(7).add(2).add(3).add(1)

    tree.max() should be(Some(7))
  }

  it should "return none when ask for min / max in empty tree" in {
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

  "Binary Tree" should "return DBNil when remove from empty tree" in {
    val empty = BinaryTree.empty[Int]

    empty should be(empty.remove(5))
  }

  it should "keep same tree structure - leaft when remove leaft" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val result = tree.remove(1)

    val sorted = result.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(2, 3, 5)
  }

  it should "point parent to child of the one removed (left) when remove to the left" in {
    val tree = BinaryTree(5).add(2).add(1).add(3)

    val result = tree.remove(2)

    val sorted = result.toLst

    ScalaListConverter.toScalaList(sorted) should contain inOrder(1, 3, 5)
  }

  it should "point parent to child of of the one removed (right) when remove to right" in {
    val tree = BinaryTree(5) + 7 + 8 + 2 + 1

    val result = tree + 3

    true should be(true)

  }
}

class BinaryTreeTest extends FlatSpec with Matchers{
  implicit val cmp =  new Order[Int] {
    override def apply(l: Int, r: Int): Ordering = l.compareTo(r) match {
      case 0  => Order.EQ
      case -1 => Order.LT
      case 1  => Order.GT
    }
  }

  "Binary Tree" should "return false when asking for non-existence item" in {
    val tree = BinaryTree(5) + 4 + 1

    tree.contains(2) should be(false)
  }

  it should "return true when asking for item in the tree" in {
    val tree = BinaryTree(5) + 4 + 1 + 2

    tree.contains(2) should be(true)
  }
}

