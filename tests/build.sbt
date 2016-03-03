import ScoverageSbtPlugin.ScoverageKeys._

name := "dogs-tests"

libraryDependencies ++= Seq (
  "org.typelevel"  %%% "cats-laws"  % "0.4.1",
  "org.scalacheck" %%% "scalacheck" % "1.12.4",
  "org.scalatest"  %%% "scalatest"  % "3.0.0-M7" % "test",
  "org.typelevel"  %%% "discipline" % "0.4"      % "test",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)

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
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Yno-imports",
  "-Yno-predef")

scalacOptions in (Compile, console) ~= (_.filterNot(Set("-Ywarn-unused-import","-Yno-imports")))
scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))


coverageExcludedPackages := "dogs\\.tests\\.arbitrary\\..*"
