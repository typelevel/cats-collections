name := "dogs-core"

libraryDependencies ++= Seq(
  "org.typelevel"                  %%% "cats-core"  % "0.4.1",
  "com.github.mpilquist"           %%% "simulacrum" % "0.7.0",
  "org.typelevel"                  %%% "machinist"  % "0.4.1",
  "com.github.mpilquist"           %%% "simulacrum" % "0.5.0",
  "org.scalacheck"                 %%% "scalacheck" % "1.12.5",

  compilerPlugin("org.spire-math"  %% "kind-projector" % "0.6.3"),
  compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.full)
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
scalacOptions in Test ~= (_.filterNot(Set("-Ywarn-unused-import","-Yno-imports", "-Yno-predef")))

doctestWithDependencies := false

doctestSettings

