name := "dogs-tests"

libraryDependencies ++= Seq (
  "org.spire-math" %%% "cats"       % "0.3.0-SNAPSHOT",
  "org.scalatest"  %%% "scalatest"  % "3.0.0-M7" % "test",
  "org.scalacheck" %%% "scalacheck" % "1.12.4"   % "test",
  "org.typelevel"  %%% "discipline" % "0.4"      % "test",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)
