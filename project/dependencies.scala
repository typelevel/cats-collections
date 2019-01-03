import sbt._

object V {
  val cats = "1.5.0"

  def scalaCheckVersion(scalaVersion: String): String =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => "1.13.5"
      case _ => "1.14.0"
    }
}
