import ScoverageSbtPlugin.ScoverageKeys._

moduleName := "dogs-bench"

publish := ()
publishLocal := ()
publishArtifact := false

enablePlugins(JmhPlugin)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

fork in run := true

coverageExcludedPackages := "cats\\.bench\\..*"
