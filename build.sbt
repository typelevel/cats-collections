name := "dogs"

scalaVersion := "2.11.2"

scalacOptions := Seq(
  "-feature",
  "-deprecation",
  "-encoding", "utf8",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-target:jvm-1.7",
  "-unchecked",
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard")

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.3.0-SNAPSHOT",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)
