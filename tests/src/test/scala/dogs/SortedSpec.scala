/**
  * Created by anicolaspp on 4/10/16.
  */

package dogs

import Predef._
import com.oracle.webservices.internal.api.message.PropertySet.Property
import dogs.tests.DietSpec._
import dogs.tests.DogsSuite
import dogs.tests.arbitrary.all._
import cats._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.Iterable
import scala.collection.immutable.{Nil,List=>SList,::}
import algebra.Eq
import algebra.std.int._
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, CartesianTests}
import cats.laws.discipline.arbitrary._

class SortedSpec extends DogsSuite {
  test("quicksort")(
    forAll{ xs: List[Int] =>
      val sorted = Sorted.quickSort.sorted(xs)

      sorted.toScalaList should be(xs.toScalaList.sortWith((x,y) => x <= y))
    }
  )

  test("heapsort")(
    forAll{ xs: List[Int] =>
      val sorted = Sorted.heapSort.sorted(xs)

      sorted.toScalaList should be(xs.toScalaList.sortWith((x,y) => x <= y))
    }
  )
}

class PartitionerSpec extends DogsSuite {

  implicit val orderBoolean = new Order[Boolean] {
    override def compare(x: Boolean, y: Boolean): Int = if (x == y) 0 else if (!x & y) -1 else 1
  }

  test("partitions")(
    forAll { xs: Set[Int] =>

      val partitions = Set.toPartition.partitions(xs)(i => i % 2 == 0)

      val xT = partitions.filter {case (b, i) => b}.flatMap(_._2)
      val xF = partitions.filter {case (b, i) => !b}.flatMap(_._2)



//      val (x, y) = xs.toPartition(i => i % 2 == 0)

      xs.toList.foreach(i => {
        if (i % 2 == 0) xT.contains(i) should be(true)
        else xF.contains(i) should be (true)
      })
    }
  )
}

//to force travis build
