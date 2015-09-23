name := "dogs"

scalaVersion := "2.11.7"

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

resolvers += 
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.spire-math" %%% "cats"       % "0.3.0-SNAPSHOT",
  "org.scalatest"  %%% "scalatest"  % "3.0.0-M7" % "test",
  "org.scalacheck" %%% "scalacheck" % "1.12.4"   % "test",
  "org.typelevel"  %%% "discipline" % "0.4",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)
