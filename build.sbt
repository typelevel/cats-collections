name := "dogs"

organization in Global := "org.typelevel"

scalaVersion in Global := "2.11.7"

resolvers in Global += Resolver.sonatypeRepo("snapshots")

lazy val dogs = project.in(file(".")).aggregate(dogsJVM, dogsJS).settings(publish := {})

lazy val dogsJVM = project.aggregate(coreJVM, docs, testsJVM, bench)
lazy val dogsJS = project.aggregate(coreJS, testsJS)

lazy val core = crossProject.crossType(CrossType.Pure)
  .jsSettings(commonJsSettings:_*)


lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val tests = crossProject.crossType(CrossType.Pure) dependsOn core
  .jsSettings(commonJsSettings:_*)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project dependsOn coreJVM

lazy val bench = project dependsOn coreJVM

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  // Using Rhino as jsEnv to build scala.js code can lead to OOM, switch to PhantomJS by default
  scalaJSUseRhino := false,
  requiresDOM := false,
  jsEnv := NodeJSEnv().value,
  // Only used for scala.js for now
  botBuild := sys.props.getOrElse("CATS_BOT_BUILD", default="false") == "true",
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(botBuild.value)
)

addCommandAlias("buildJVM", ";coreJVM/compile;coreJVM/test;testsJVM/test;bench/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;makeSite")

addCommandAlias("validateJS", ";coreJS/compile;testsJS/test")

addCommandAlias("validate", ";validateJS;validateJVM")
