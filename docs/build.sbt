import microsites._

name := "cats-collections-docs"

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

enablePlugins(MicrositesPlugin)

ghpagesNoJekyll := false
micrositeName := "cats-collections"
micrositeDescription := "pure functional data structures for Scala"
micrositeBaseUrl := "/cats-collections/"
micrositeHomepage := "http://typelevel.org/cats-collections/"
micrositeGithubOwner := "typelevel"
micrositeGithubRepo := "cats-collections"
micrositeExtraMdFiles := Map(
  file("README.md") -> ExtraMdFileConfig(
    "index.md",
    "docs",
    Map("title" -> "Home", "layout" -> "docs")
  )
)

micrositePalette := Map(
  "brand-primary" -> "#5B5988",
  "brand-secondary" -> "#292E53",
  "brand-tertiary" -> "#222749",
  "gray-dark" -> "#49494B",
  "gray" -> "#7B7B7E",
  "gray-light" -> "#E5E5E6",
  "gray-lighter" -> "#F4F3F4",
  "white-color" -> "#FFFFFF")

includeFilter in Jekyll := (includeFilter in makeSite).value

fork in tut := true

git.remoteRepo := "git@github.com:typelevel/cats-collections.git"

scalacOptions := Seq(
  "-feature",
  "-deprecation",
  "-encoding", "utf8",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-nowarn")
