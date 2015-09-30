package dogs.bedazzle

import scala.collection.immutable.{List,Nil}

trait ListBedazzle {
  def nil[A]: List[A] = Nil
}
