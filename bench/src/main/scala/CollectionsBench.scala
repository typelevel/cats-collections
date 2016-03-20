package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scala.collection.immutable.{List => SList, Nil => SNil}
import scalaz.{DList => SZDList, IList}
import cats.Eval

@State(Scope.Thread)
class Append {
  val listOfLists: SList[SList[Int]] =
    (1 to 10000).toList.grouped(10).toList

  val listOfListsDogs: SList[List[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(List.fromIterable)

  val listOfDLists: SList[DList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(x => DList(List.fromIterable(x)))

  val listOfSZDLists: SList[SZDList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(SZDList.apply)

  @Benchmark def dogsListAppend(): Unit = {
    listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
  }

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
class AppendThenToList {
  val listOfLists: SList[SList[Int]] =
    (1 to 10000).toList.grouped(10).toList

  val listOfListsDogs: SList[List[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(List.fromIterable)

  val listOfDLists: SList[DList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(x => DList(List.fromIterable(x)))

  val listOfSZDLists: SList[SZDList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(SZDList.apply)

  @Benchmark def dogsList(): Unit = {
    listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
  }

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
class AppendThenIterate {
  val listOfLists: SList[SList[Int]] =
    (1 to 10000).toList.grouped(10).toList

  val listOfListsDogs: SList[List[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(List.fromIterable)

  val listOfDLists: SList[DList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(x => DList(List.fromIterable(x)))

  val listOfSZDLists: SList[SZDList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(SZDList.apply)

  @Benchmark def dogsList(): Unit = {
    val l = listOfListsDogs.foldLeft[List[Int]](List.empty)(_ ++ _)
    l.foldLeft(())((x, y) => ())
  }

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
class AppendThenHeadOption {

  val listOfLists: SList[SList[Int]] =
    (1 to 10000).toList.grouped(10).toList

  val listOfDLists: SList[DList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(x => DList(List.fromIterable(x)))

  val dogsdl = listOfDLists.foldLeft[DList[Int]](DList.empty)(_ ++ _)

  val listOfSZDLists: SList[SZDList[Int]] =
    (1 to 10000).toList.grouped(10).toList.map(SZDList.apply)

  val scaalzdl = listOfSZDLists.foldLeft[SZDList[Int]](SZDList(1))(_ ++ _)

  val dequeue = listOfLists.foldLeft[Dequeue[Int]](Dequeue.empty)((dq,li) =>
      li.foldLeft(dq)(_ :+ _)
    )


  @Benchmark def dogsDListHeadOption(): Unit = {
    dogsdl.headOption
  }

  @Benchmark def scalazDListHeadOption(): Unit = {
    scaalzdl.headOption
  }

  @Benchmark def dequeueHeadOption(): Unit = {
    dequeue.frontOption
  }
}


