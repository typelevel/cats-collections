package dogs.syntax

import scala.collection.immutable.{List,Nil}

trait ListBedazzle {
  def nil[A]: List[A] = Nil
}
