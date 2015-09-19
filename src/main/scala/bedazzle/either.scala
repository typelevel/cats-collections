package dogs.bedazzle

trait EitherBedazzle {
  implicit def eitherBedazzled[A](a: A): EitherBedazzled[A] = new EitherBedazzled(a)
}

class EitherBedazzled[A](val a: A) extends AnyVal {
  def left: Either[A, Nothing] = Left(a)
  def right: Either[Nothing, A] = Right(a)
}
