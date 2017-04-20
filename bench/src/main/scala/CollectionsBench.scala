package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import scala.collection.immutable.{List => SList, Nil => SNil}
import scalaz.{DList => SZDList, IList}
import cats.Eval

trait Lists {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var listOfLists: SList[SList[Int]] = _
//  var listOfListsDogs: SList[List[Int]] = _
  var listOfDLists: SList[DList[Int]] = _
  var listOfSZDLists: SList[SZDList[Int]] = _
  var dogsdl: DList[Int] = _
  var scaalzdl: SZDList[Int] = _
  var dequeuel: Dequeue[Int] = _

  @Setup
  def setup: Unit = {
    listOfLists = (1 to n).toList.grouped(10).toList
//    listOfListsDogs = (1 to n).toList.grouped(10).toList.map(List.fromIterable)
    listOfDLists = (1 to n).toList.grouped(10).toList.map(x => DList(x))
    listOfSZDLists = (1 to n).toList.grouped(10).toList.map(SZDList.apply)
    dogsdl = listOfDLists.foldLeft[DList[Int]](DList.empty)(_ ++ _)
    scaalzdl = listOfSZDLists.foldLeft[SZDList[Int]](SZDList(1))(_ ++ _)
    dequeuel = listOfLists.foldLeft[Dequeue[Int]](Dequeue.empty)((dq,li) =>
      li.foldLeft(dq)(_ :+ _)
    )
  }
}

@State(Scope.Thread)
class Append extends Lists {

//  @Benchmark def dogsListAppend(): Unit = {
//    listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
//  }

  @Benchmark def dogsDListAppend(): Unit = {
    listOfDLists.foldLeft[DList[Int]](DList.empty)(_ ++ _)
  }

  @Benchmark def scalazDListAppend(): Unit = {
    listOfSZDLists.foldLeft[SZDList[Int]](SZDList(1))(_ ++ _)
  }

  @Benchmark def dequeueAppend(): Unit = {
    listOfLists.foldLeft[Dequeue[Int]](Dequeue.empty)((dq,li) =>
      li.foldLeft(dq)(_ :+ _)
    )
  }
}

@State(Scope.Thread)
class AppendThenToList extends Lists {
//  @Benchmark def dogsList(): Unit = {
//    listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
//  }

  @Benchmark def dogsDList(): Unit = {
    listOfDLists.foldLeft[DList[Int]](DList.empty)(_ ++ _).toList
  }

  @Benchmark def scalaz(): Unit = {
    listOfSZDLists.foldLeft[SZDList[Int]](SZDList(1))(_ ++ _).toList
  }

  @Benchmark def dequeue(): Unit = {
    listOfLists.foldLeft[Dequeue[Int]](Dequeue.empty)((dq,li) =>
      li.foldLeft(dq)(_ :+ _)
    ).foldRight[List[Int]](Eval.now(List.empty))((a,las) => las.map(_.::(a)))
  }
}

@State(Scope.Thread)
class AppendThenIterate extends Lists {
//  @Benchmark def dogsList(): Unit = {
//    val l = listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
//    l.foldLeft(())((x, y) => ())
//  }

  @Benchmark def dogsDList(): Unit = {
    val l = listOfDLists.foldLeft[DList[Int]](DList.empty)(_ ++ _)
    def unc(a: Int, dl: DList[Int]): Eval[Unit] =
      Eval.defer(Eval.now(dl.uncons(Eval.now(()), unc)))

    l.uncons(Eval.now(()), unc).value
  }

  @Benchmark def scalaz(): Unit = {
    val l = listOfSZDLists.foldLeft[SZDList[Int]](SZDList(1))(_ ++ _)
     def unc(a: Int, dl: SZDList[Int]): Unit = dl.uncons((), unc)
    l.uncons((), unc)
  }

  @Benchmark def dequeue(): Unit = {
    val l = listOfLists.foldLeft[Dequeue[Int]](Dequeue.empty)((dq,li) =>
      li.foldLeft(dq)(_ :+ _)
    )

    def unc(o: Option[(Int,Dequeue[Int])]): Unit = o match {
      case Some((_, o)) => unc(o.uncons)
      case _ => ()
    }

    unc(l.uncons)
  }
}


@State(Scope.Thread)
class AppendThenHeadOption extends Lists {
  @Benchmark def dogsDListHeadOption(): Unit = {
    dogsdl.headOption
  }

  @Benchmark def scalazDListHeadOption(): Unit = {
    scaalzdl.headOption
  }

  @Benchmark def dequeueHeadOption(): Unit = {
    dequeuel.frontOption
  }
}


