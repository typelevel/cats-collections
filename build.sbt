name := "dogs"

organization in Global := "org.typelevel"

scalaVersion in Global := "2.11.7"

resolvers in Global += Resolver.sonatypeRepo("snapshots")

(licenses in Global) += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

scmInfo := Some(ScmInfo(url("https://github.com/stew/dogs"),
  "https://github.com/stew/dogs.git"))

lazy val dogs = project.in(file(".")).aggregate(dogsJVM, dogsJS).settings(publish := {})

lazy val dogsJVM = project.aggregate(coreJVM, docs, testsJVM, bench).settings(publish := {})
lazy val dogsJS = project.aggregate(coreJS, testsJS).settings(publish := {})

lazy val core = crossProject.crossType(CrossType.Pure)
  .jsSettings(commonJsSettings:_*)


lazy val coreJVM = core.jvm.settings(publishSettings)
lazy val coreJS = core.js.settings(publishSettings)

lazy val tests = crossProject.crossType(CrossType.Pure) dependsOn core
  .jsSettings(commonJsSettings:_*)

lazy val testsJVM = tests.jvm.settings(publish := {})
lazy val testsJS = tests.js.settings(publish := {})

lazy val docs = project.dependsOn(coreJVM).settings(publish := {})

lazy val bench = project.dependsOn(coreJVM).settings(publish := {})

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


lazy val tagName = Def.setting{
 s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val publishSettings = Seq(
  bintrayRepository := "releases",
  releaseCrossBuild := true,
  releaseTagName := tagName.value,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false)
)
