package dogs

import cats.data._
import Maybe._
import Streaming._

object streaming {
  def unfold[A,B](b: B)(f: B => Maybe[(A,B)]): Streaming[A] = f(b) match {
    case NotThere()   => Streaming.empty
    case There((a,b)) => Streaming.cons(a, defer(unfold(b)(f)))
  }
}
