package dogs
import Predef._
import java.lang.Throwable
import scala.Null
import scala.reflect.ClassTag
import java.util.concurrent.CountDownLatch

/**
 * A Task is an abstraction of a computation which produces an A
 * possibly asynchronously
 */
sealed trait Task[A] {
  import Task._

  protected def eval: Eval[A]

  def map[B](f: A => B): Task[B] =
    flatMap(a => Task.now(f(a)))

  def flatMap[B](f: A => Task[B]): Task[B] =
    new Bind(this, f)

  def catchOnly[T >: Null <: Throwable: ClassTag]: Task[T Xor A] =
    new AttemptOnly[T, A](this)

  def catchNonFatal: Task[Throwable Xor A] =
    new AttemptNonFatal(this)

  final def unsafePerformIO(): A = eval.value
}

object Task {
  /**
   * Construct a Task which represents an already calculated eager
   * value
   */
  def now[A](a: A): Task[A] = Value(Eval.now(a))

  /**
   * Construct a Task that when run will produce an A by evaluating
   * the argument. This is useful for a pure but expensive computation
   */
  def later[A](a: => A): Task[A] = Value(Eval.later(a))

  /**
   * Construct a task from a thunk that will be executed each time the
   * Task is run. This is an effect capturing constructor which is
   * suitable for wrapping code which is side-effecting.
   */
  def always[A](a: () => A): Task[A] = Value(new Always(a))

  /**
   *  Construct a Task which will represent an asynchronous
   *  computation. The constructor takes a function which will be
   *  invoked when the Task is run, passing the task a callback (A =>
   *  Unit) which will be called when the asynchronous computation is
   *  complete.
   */
  def async[A](cb: (A => Unit) => Unit): Task[A] = Async(cb)

  // Here we already have an eval, so we just have to return it
  private[dogs] final case class Value[A](eval: Eval[A]) extends Task[A] {
    override def map[B](f: A => B): Task[B] = Value(eval.map(f))
  }

  // Store a flatMap operation on the Heap. when asked to eval, we
  // flatMap against the given task, using Eval.defer to ensure that
  // the computation is stack-safe
  private[dogs] final class Bind[Z, A](t: Task[Z], f: Z => Task[A]) extends Task[A] {
    override def eval: Eval[A] = Eval.defer(t.eval.flatMap(f andThen (_.eval)))
  }

  private[dogs] final case class AttemptOnly[T >: Null <: Throwable : ClassTag, A](task: Task[A]) extends Task[T Xor A] {
    override def eval: Eval[Xor[T, A]] =
      Eval.always(Xor.catchOnly[T](task.eval.value))
  }

  private[dogs] final case class AttemptNonFatal[A](task: Task[A]) extends Task[Xor[Throwable, A]] {
    override def eval: Eval[Xor[Throwable, A]] =
      Eval.always(Xor.catchNonFatal(task.eval.value))
  }

  private[dogs] final case class Async[A](complete: (A => Unit) => Unit) extends Task[A] {
    override def eval: Eval[A] = {
      val cdl = new CountDownLatch(1)
      var result: Option[A] = Option.none
      complete({ (a: A) => result = Some(a); cdl.countDown })
      cdl.await
      result match {
        case Some(a) => Eval.now(a)
        case _ => throw new java.lang.Exception("can't happen")
      }
    }
  }
}
