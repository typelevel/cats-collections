import microsites._

name := "dogs-docs"

//enablePlugins(TutPlugin, GhpagesPlugin)

//tutTargetDirectory := siteDirectory.value / "tut"

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

enablePlugins(MicrositesPlugin)

ghpagesNoJekyll := false
micrositeName := "dogs"
micrositeBaseUrl := "/dogs"
micrositeHomepage := "https://stew.github.io/dogs/"
micrositeGithubOwner := "stew"
micrositeGithubRepo := "dogs"
micrositeHighlightTheme := "atom-one-light"
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

autoAPIMappings := true

docsMappingsAPIDir := "api"

includeFilter in Jekyll := (includeFilter in makeSite).value

fork in tut := true

git.remoteRepo := "git@github.com:stew/dogs.git"

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
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture")

