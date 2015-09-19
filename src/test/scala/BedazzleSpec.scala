package dogs

import org.scalacheck._
import org.scalacheck.Prop._

object BedazzleSpec extends Properties("Bedazzle") {
  property("some") = secure {
    import bedazzle.all._
    1.some == Some(1)
  }
  property("some") = secure {
    import bedazzle.all._
    none[String] == Option.apply[String](null)
  }
}
