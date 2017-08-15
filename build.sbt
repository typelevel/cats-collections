import ReleaseTransformations._

lazy val buildSettings = Seq(
  name := "dogs",
  organization in Global := "org.typelevel",
  scalaVersion in Global := "2.12.3",
  crossScalaVersions in Global := Seq("2.11.11", scalaVersion.value)
  //resolvers in Global += Resolver.sonatypeRepo("snapshots")
)

lazy val dogs = project.in(file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings)
  .aggregate(dogsJVM, dogsJS)

lazy val dogsJVM = project.in(file(".dogsJVM"))
  .settings(moduleName := "dogs")
  .settings(noPublishSettings)
  .aggregate(coreJVM, docs, testsJVM, bench)

lazy val dogsJS = project.in(file(".dogsJS"))
  .settings(moduleName := "dogs")
  .settings(noPublishSettings)
  .aggregate(coreJS, testsJS)

lazy val core = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "dogs-core")
  .settings(dogsSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val tests = crossProject.crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "dogs-tests")
  .settings(dogsSettings:_*)
  .settings(
    coverageEnabled := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-laws"          % "1.0.0-MF",
      "org.scalacheck" %% "scalacheck"         % "1.13.4",
      "org.scalatest"  %% "scalatest"          % "3.0.0"    % "test",
      "org.typelevel"  %% "catalysts-platform" % "0.0.5"    % "test",
      "org.typelevel"  %% "discipline"         % "0.7.3"    % "test"
    )
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

lazy val docs = project
  .dependsOn(coreJVM)
  .enablePlugins(TutPlugin)
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)

lazy val bench = project
  .settings(moduleName := "dogs-bench")
  .dependsOn(coreJVM)
  //.settings(dogsSettings:_*)
  .settings(noPublishSettings)
  .settings(
    coverageEnabled := false,
    fork in run := true,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
  )
  .enablePlugins(JmhPlugin)

lazy val botBuild = settingKey[Boolean]("Build by TravisCI instead of local dev environment")

lazy val dogsSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val commonSettings = compilerFlags ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel"                  %% "cats-core"  % "1.0.0-MF",
    "com.github.mpilquist"           %% "simulacrum" % "0.10.0",
    "org.typelevel"                  %% "machinist"  % "0.6.1",

    compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.3"),
    compilerPlugin("org.scalamacros" %% "paradise"       % "2.1.0" cross CrossVersion.patch)
  ),
  fork in test := true
  // parallelExecution in Test := false,
)

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  requiresDOM := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // Only used for scala.js for now
  botBuild := scala.sys.env.get("TRAVIS").isDefined,
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(botBuild.value)
)

addCommandAlias("buildJVM", ";coreJVM/compile;coreJVM/test;testsJVM/test;bench/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;makeSite")

//addCommandAlias("validateJS", ";coreJS/compile;testsJS/test")

//addCommandAlias("validate", ";validateJS;validateJVM")
addCommandAlias("validate", ";validateJVM")

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

lazy val compilerFlags = Seq(
  scalacOptions ++= Seq(
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Yno-imports",                      // No predef or default imports
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ),
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 11 =>
        Seq("-Xlint")
      case _ =>
        Seq(
          "-explaintypes",                     // Explain type errors in more detail.
          "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
          "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
          "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
          "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
          "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
          "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
          "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
          "-Xlint:option-implicit",            // Option.apply used implicit view.
          "-Xlint:package-object-classes",     // Class or object defined in package object.
          "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
          "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
          "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
          "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
          "-Xlint:unsound-match",              // Pattern match may not be typesafe.
          "-Ypartial-unification",             // Enable partial unification in type constructor inference
          "-Ywarn-dead-code",                  // Warn when dead code is identified.
          "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
          "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
          "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
          "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals",              // Warn if a local definition is unused.
          "-Ywarn-unused:params",              // Warn if a value parameter is unused.
          // "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates"            // Warn if a private member is unused.
        )
    }
  ),
  scalacOptions in (Test, compile) --= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 11 =>
        Seq("-Yno-imports")
      case _ =>
        Seq(
          "-Ywarn-unused:privates",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Yno-imports"
        )
    }
  ),
  scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports"),
  scalacOptions in (Tut) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports"),
  scalacOptions in (Tut) ++= Seq("-Yno-predef")
)
