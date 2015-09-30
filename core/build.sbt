name := "dogs-core"

libraryDependencies ++= Seq(
  "org.spire-math" %%% "cats"       % "0.3.0-SNAPSHOT",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)
