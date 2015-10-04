name := "dogs-tests"

libraryDependencies ++= Seq (
  "org.spire-math" %%% "cats"       % "0.3.0-SNAPSHOT",
  "org.scalacheck" %%% "scalacheck" % "1.12.4",
  "org.scalatest"  %%% "scalatest"  % "3.0.0-M7" % "test",
  "org.typelevel"  %%% "discipline" % "0.4"      % "test",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)
