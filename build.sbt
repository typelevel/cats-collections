import ReleaseTransformations._

name := "dogs"

organization in Global := "org.typelevel"

scalaVersion in Global := "2.11.7"

resolvers in Global += Resolver.sonatypeRepo("snapshots")

lazy val dogs = project.in(file(".")).aggregate(dogsJVM, dogsJS).settings(noPublishSettings)

lazy val dogsJVM = project.aggregate(coreJVM, docs, testsJVM, bench).settings(noPublishSettings)
lazy val dogsJS = project.aggregate(coreJS, testsJS).settings(noPublishSettings)

lazy val core = crossProject.crossType(CrossType.Pure)
  .jsSettings(commonJsSettings:_*)


lazy val coreJVM = core.jvm.settings(publishSettings)
lazy val coreJS = core.js.settings(publishSettings)

lazy val tests = crossProject.crossType(CrossType.Pure) dependsOn core
  .jsSettings(commonJsSettings:_*)

lazy val testsJVM = tests.jvm.settings(noPublishSettings)
lazy val testsJS = tests.js.settings(noPublishSettings)

lazy val docs = project.dependsOn(coreJVM).settings(noPublishSettings)

lazy val bench = project.dependsOn(coreJVM).settings(noPublishSettings)

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

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)


lazy val tagName = Def.setting{
 s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
    pushChanges)
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releaseTagName := tagName.value,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("https://github.com/stew/dogs")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  scmInfo := Some(ScmInfo(url("https://github.com/stew/dogs"), "scm:git:git@github.com:stew/dogs.git")),
  autoAPIMappings := true,
  pomExtra := (
    <developers>
      <developer>
        <name>Stew O'Connor</name>
        <url>https://github.com/stew/</url>
      </developer>
      <developer>
        <id>anicolaspp</id>
        <name>Nicolas A Perez</name>
        <url>https://github.com/anicolaspp/</url>
      </developer>
    </developers>
  )
) ++ credentialSettings ++ sharedReleaseProcess
