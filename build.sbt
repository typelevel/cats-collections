import ReleaseTransformations._

lazy val buildSettings = Seq(
  name := "dogs",
  organization in Global := "org.typelevel",
  scalaVersion in Global := "2.12.3",
  crossScalaVersions := Seq("2.11.11", scalaVersion.value)
)

lazy val dogs = project.in(file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings)
  .aggregate(core, tests, docs, bench)

lazy val core = project
  .settings(moduleName := "dogs-core")
  .settings(dogsSettings:_*)
  .settings(publishSettings)

lazy val tests = project
  .dependsOn(core)
  .settings(moduleName := "dogs-tests")
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)
  .settings(coverageEnabled := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-laws"          % "1.0.0-MF",
      "org.scalacheck" %% "scalacheck"         % "1.13.4",
      "org.scalatest"  %% "scalatest"          % "3.0.0"    % "test",
      "org.typelevel"  %% "catalysts-platform" % "0.0.5"    % "test",
      "org.typelevel"  %% "discipline"         % "0.7.3"    % "test"
    )
  )

lazy val docs = project
  .dependsOn(core)
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)

lazy val bench = project
  .settings(moduleName := "dogs-bench")
  .dependsOn(core)
  .settings(noPublishSettings)
  .settings(
    coverageEnabled := false,
    fork in run := true,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
  )
  .enablePlugins(JmhPlugin)

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val dogsSettings = buildSettings ++ commonSettings ++ scoverageSettings

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  libraryDependencies ++= Seq(
    "org.typelevel"                  %% "cats-core"  % "1.0.0-MF",
    "com.github.mpilquist"           %% "simulacrum" % "0.10.0",
    "org.typelevel"                  %% "machinist"  % "0.6.1",

    compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.3"),
    compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.patch)
  ),
  fork in test := true,
  // parallelExecution in Test := false,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings")
) ++ warnUnusedImport

addCommandAlias("build", ";core/compile;core/test;tests/test")
addCommandAlias("validate", ";scalastyle;build;makeSite")

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

lazy val publishSettings = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion),//,
//    commitReleaseVersion,
//    tagRelease,
//    publishArtifacts,
//    setNextVersion,
//    commitNextVersion,
//    releaseStepCommand("sonatypeReleaseAll"),
//    pushChanges),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
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
) ++ credentialSettings

lazy val commonScalacOptions = Seq(
  "-feature",
  "-deprecation",
  "-encoding", "utf8",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture")

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
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)
