import com.typesafe.sbt.SbtSite.SiteKeys._
//import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
//import sbtunidoc.Plugin.UnidocKeys._


name := "dogs-docs"

enablePlugins(JekyllPlugin, TutPlugin, GhpagesPlugin)

//addMappingsToSiteDir(tut, (tutTargetDirectory in Tut))

ghpagesNoJekyll := false

includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.yml" | "*.md"

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
  "-Xfuture",
  "-Yno-predef")

