package dogs
package bench

import dogs.Order.Ordering
import dogs.std._
import org.openjdk.jmh.annotations._

import Predef._

import scala.collection.immutable.IndexedSeq
import scala.util.Random
import scalaz.{OrderingInstances, Diev}


@State(Scope.Benchmark)
class ListPrependBench {

  @Benchmark
  def dogsSum(): Int = {
    var i = 0
    var l: dogs.List[Int] = dogs.El()
    while(i < 100000) {
      l = i :: l
      i = i + 1
    }

    def sum(l: List[Int], s: Int): Int = l match {
      case El() => s
      case h Nel t => sum(t, s + h)
    }

    sum(l, 0)
  }

  @Benchmark
  def scalaSum(): Int = {
    var i = 0
    var l: scala.List[Int] = scala.Nil
    while(i < 100000) {
      l = i :: l
      i = i + 1
    }

    def sum(l: scala.List[Int], s: Int): Int = l match {
      case Nil => s
      case h :: t => sum(t, s + h)
    }

    sum(l, 0)
  }

}


object DietDataGen {

  def getWorstCaseData: scala.IndexedSeq[Int] = {
    var x = 0

    for (x <- scala.Range(1, 1000)
      if (x % 2 == 0)
    ) yield x
  }

  def getBestCaseData: scala.IndexedSeq[scala.Range] = {
    var x = 0

    for (x <- scala.Range(1, 1000)
         if (x % 10 == 0)
    ) yield scala.Range(x, x + 10)
  }
}

@State(Scope.Benchmark)
class DietAddBench {

  implicit object EnumInt extends Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = intOrder(l,r)
  }

  implicit object scalazEnumInt extends scalaz.Enum[Int] {
    override def succ(a: Int): Int = a + 1

    override def pred(a: Int): Int = a - 1

    override def order(x: Int, y: Int): scalaz.Ordering = {
      if (x == y) scalaz.Ordering.EQ
      else if (x < y) scalaz.Ordering.LT
      else scalaz.Ordering.GT
    }
  }

  var items: Seq[Int] = Seq[Int]()
  var ranges = Seq[scala.Range]()
  var toSearch = Seq[Int]()

  @Setup
  def setup: Unit = {
    items = DietDataGen.getWorstCaseData

    toSearch = Random.shuffle(items)

    ranges = DietDataGen.getBestCaseData
  }

  @Benchmark
  def dogsDietAdd: Unit = {
    var diet = dogs.Diet.empty[Int]

    items.foreach{ i=>
      diet = diet + i
    }
  }

  @Benchmark
  def scalazDievAdd: Unit = {
    var diev = Diev.empty[Int]

    items.foreach{i =>
      diev = diev + i
    }
  }

  @Benchmark
  def dogsDietAddRange: Unit = {
    var diet = dogs.Diet.empty[Int]

    ranges.foreach {r =>
      diet = diet +  Range(r.start, r.end)
    }
  }

  @Benchmark
  def scalazDievAddRange: Unit = {
    var diev = scalaz.Diev.empty[Int]

    ranges.foreach {r =>
      diev = diev + (r.start, r.end)
    }
  }


}

@State(Scope.Benchmark)
class DietBenchSearch {

  implicit object EnumInt extends Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = intOrder(l,r)
  }

  implicit object scalazEnumInt extends scalaz.Enum[Int] {
    override def succ(a: Int): Int = a + 1

    override def pred(a: Int): Int = a - 1

    override def order(x: Int, y: Int): scalaz.Ordering = {
      if (x == y) scalaz.Ordering.EQ
      else if (x < y) scalaz.Ordering.LT
      else scalaz.Ordering.GT
    }
  }

  var diet = Diet.empty[Int]
  var diev = Diev.empty[Int]

  @Setup
  def setup: Unit = {
    var i = 0
    while (i < 1000) {
      val s = Random.nextInt()
      val e = s + Random.nextInt()

      diet = diet + Range(s, e)
      diev = diev + (s, e)

      i = i + 1
    }
  }

  @Benchmark
  def dogsDietSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diet.contains(i))
  }

  @Benchmark
  def scalazDievSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diev.contains(i))
  }
}