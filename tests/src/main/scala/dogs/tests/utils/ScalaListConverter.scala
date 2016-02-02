package dogs.tests.utils

import scala.annotation.tailrec

/**
 * Created by nperez on 1/30/16.
 */
object ScalaListConverter {
  implicit def toScalaList[A](ls: dogs.List[A]): List[A] = {
   def traverse(xs: dogs.List[A]): List[A] = xs match {
     case dogs.El()       =>  Nil
     case dogs.Nel(h, t)  =>  h :: traverse(t)
   }

    traverse(ls)
  }
}
