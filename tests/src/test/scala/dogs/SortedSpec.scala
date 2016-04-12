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
      val sorted = Sorted.quickSort(xs).sorted

      sorted.toScalaList should be(xs.toScalaList.sortWith((x,y) => x <= y))
    }
  )

  test("heapsort")(
    forAll{ xs: List[Int] =>
      val sorted = Sorted.heapSort(xs).sorted

      sorted.toScalaList should be(xs.toScalaList.sortWith((x,y) => x <= y))
    }
  )
}

class PartitionerSpec extends DogsSuite {
  test("partitions")(
    forAll { xs: List[Int] =>
      val (x, y) = BiPartition(xs).partition(i => i % 2 == 0)

      xs.foreach(i => {
        if (i % 2 == 0) x.contains(i) should be(true)
        else y.contains(i) should be (true)
      })
    }
  )
}

//to force travis build
