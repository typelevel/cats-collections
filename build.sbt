import ReleaseTransformations._

lazy val buildSettings = Seq(
  name := "dogs",
  organization in Global := "org.typelevel",
  scalaVersion in Global := "2.11.8"
  //resolvers in Global += Resolver.sonatypeRepo("snapshots")
)

lazy val dogs = project.in(file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings)
  .aggregate(dogsJVM, dogsJS)

lazy val dogsJVM = project.in(file(".catsJVM"))
  .settings(moduleName := "dogs")
  .settings(noPublishSettings)
  .aggregate(coreJVM, docs, testsJVM, bench)

lazy val dogsJS = project.in(file(".catsJS"))
  .settings(moduleName := "dogs")
  .settings(noPublishSettings)
  .aggregate(coreJS, testsJS)

lazy val core = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "dogs-core")
  .settings(dogsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val tests = crossProject.crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(dogsSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(
    coverageEnabled := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.typelevel"  %%% "cats-laws"          % "0.7.2",
      "org.scalacheck" %%% "scalacheck"         % "1.12.5",
      "org.scalatest"  %%% "scalatest"          % "3.0.0-M7" % "test",
      "org.typelevel"  %%% "catalysts-platform" % "0.0.2"    % "test",
      "org.typelevel"  %%% "discipline"         % "0.4"      % "test"
    )
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project
  .dependsOn(coreJVM)
  .settings(dogsSettings:_*)
  .settings(commonJvmSettings)
  .settings(noPublishSettings)

lazy val bench = project
  .settings(moduleName := "dogs-bench")
  .dependsOn(coreJVM)
  //.settings(dogsSettings:_*)
  .settings(commonJvmSettings)
  .settings(noPublishSettings)
  .settings(
    coverageEnabled := false,
    fork in run := true,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"
  )
  .enablePlugins(JmhPlugin)

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val dogsSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  libraryDependencies ++= Seq(
    "org.typelevel"                  %%% "cats-core"  % "0.7.2",
    "com.github.mpilquist"           %%% "simulacrum" % "0.8.0",
    "org.typelevel"                  %%% "machinist"  % "0.5.0",

    compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.0"),
    compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.full)
  ),
  fork in test := true,
  // parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings")
) ++ warnUnusedImport

lazy val commonJvmSettings = commonDoctestSettings

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  // Using Rhino as jsEnv to build scala.js code can lead to OOM, switch to PhantomJS by default
  scalaJSUseRhino := false,
  requiresDOM := false,
  jsEnv := NodeJSEnv().value,
  // Only used for scala.js for now
  botBuild := scala.sys.env.get("TRAVIS").isDefined,
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(botBuild.value),
   doctestGenTests := Seq.empty,
  doctestWithDependencies := false
)

addCommandAlias("buildJVM", ";coreJVM/compile;coreJVM/test;testsJVM/test;bench/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;makeSite")

addCommandAlias("validateJS", ";coreJS/compile;testsJS/test")

addCommandAlias("validate", ";validateJS;validateJVM")

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := scalaBinaryVersion.value != "2.10"
)

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

lazy val commonScalacOptions = Seq(
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

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
)

lazy val commonDoctestSettings = Seq(
  doctestWithDependencies := false
)
