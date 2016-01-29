/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs
package tests

import dogs.Order.Ordering
import dogs.Predef._
import org.scalatest.{FlatSpec, Matchers}
import dogs.bedazzle._



class BinaryTreeTest extends FlatSpec with Matchers {

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

  "add" should "insert value recuersiverly" in {

    val branch = BinaryTree(5)
    
    val result = branch.add(7).add(6)

    result.value should be(Some(5))
    result.right.value should be(Some(7))
    result.right.left.value should be(Some(6))
  }
}

