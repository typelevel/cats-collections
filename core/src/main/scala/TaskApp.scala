package dogs

import dogs.Predef.{String, Unit}

import scala.Array
import scala.Predef.wrapRefArray

abstract class TaskApp {
  def run(args: List[String]): Task[Unit]

  final def main(args: Array[String]): Unit =
    run(List.fromIterable(args)).unsafePerformIO()
}
