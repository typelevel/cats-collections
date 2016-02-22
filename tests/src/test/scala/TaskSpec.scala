package dogs
package tests

import Predef._
import org.scalatest.{FlatSpec, Matchers}

class TaskTests extends FlatSpec with Matchers {

  "Task" should "not run side-effects until .unsafePerformIO is called" in {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    run should be (0)
    t.unsafePerformIO
    run should be (1)
  }

  it should "only evaulate side-effects once for Task.later" in {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    t.unsafePerformIO
    run should be(1)

    t.unsafePerformIO
    run should be (1)

    val t2 = t.map(_ + 1)
    t2.unsafePerformIO
    t2.unsafePerformIO

    run should be (1)

    val t3 = t.flatMap(x => Task.now(x + 1))
    t3.unsafePerformIO
    t3.unsafePerformIO

    run should be (1)
  }

  it should "not run async until run is called" in {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    run should be (0)

    t.unsafePerformIO
    run should be (1)
  }

  it should "run async run multiple times if the task is rum multiple times" in {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    run should be (0)

    t.unsafePerformIO
    run should be (1)

    t.unsafePerformIO
    run should be (2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.unsafePerformIO
    run should be (3)
  }

  it should "not run always until run is called" in {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    run should be (0)

    t.unsafePerformIO
    run should be (1)
  }

  it should "run always multiple times if the task is rum multiple times" in {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    run should be (0)

    t.unsafePerformIO
    run should be (1)

    t.unsafePerformIO
    run should be (2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.unsafePerformIO
    run should be (3)
  }

  val ones = List(Task.now(1),
                  Task.later(1),
                  Task.always(() => 1),
                  Task.async[Int](_(1)))



  it should "put success on the right when catch is called" in {
    val attempted = ones.map(_.catchNonFatal.unsafePerformIO)
    println(attempted)
    attempted.forall(_ == Xor.Right(1)) should be (true)
  }

  // an infinite stream of ones
  val onesStream: Streaming[Task[Int]] = Streaming.continually(Streaming.fromList(ones)).flatMap(identity)


  // grr
  def taskMap2[A,B,C](t1: Task[A], t2: Task[B])(f: (A,B) => C): Task[C] = {
    t1.flatMap(a => t2.map(b => f(a,b)))
  }

  // grr
  def sequenceStreaming[A](fa: Streaming[Task[A]]): Task[Streaming[A]] = {
    fa.foldRight(Eval.later(Task.now(Streaming.empty[A])))((a, st) =>
      st.map(b => taskMap2(b,a)((x,y) => Streaming.cons(y,x)))).value
  }

  it should "have a stack-safe flatMap" in {
    val howmany = 1000000

    sequenceStreaming(onesStream.take(howmany)).unsafePerformIO.foldLeft(0)((x, _) => x + 1) should be (howmany)
  }
}


