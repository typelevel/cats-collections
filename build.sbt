import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization in Global := "org.typelevel",
  scalaVersion in Global := "2.12.6",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value, "2.13.0-M5")
)

lazy val `cats-collections` = project.in(file("."))
  .settings(buildSettings:_*)
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS, bench, scalacheckJVM, scalacheckJS, testsJVM, testsJS, docs)
  .settings(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeReleaseAll"),
      setNextVersion,
      commitNextVersion,
      pushChanges))

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "cats-collections-core")
  .settings(dogsSettings:_*)
  .settings(publishSettings)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val bd = baseDirectory.value
      def extraDirs(suffix: String) =
        CrossType.Pure.sharedSrcDir(bd, "main").toList map (f => file(f.getPath + suffix))
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y <= 12 =>
          extraDirs("-2.12-")
        case Some((2, y)) if y >= 13 =>
          extraDirs("-2.13+")
        case _ => Nil
      }
    }
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "cats-collections-scalacheck")
  .settings(dogsSettings:_*)
  .settings(publishSettings)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % V.scalaCheckVersion(scalaVersion.value)
  )

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS = scalacheck.js

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(scalacheck)
  .settings(moduleName := "cats-collections-tests")
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)
  .settings(coverageEnabled := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "1000"), // "-verbosity", "2"), // increase for stress tests
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-testkit" % V.cats % "test",
      "org.typelevel" %%% "cats-laws"    % V.cats % "test",
      "org.typelevel" %%% "algebra-laws" % V.algebra % "test"
    )
  )

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project
  .dependsOn(coreJVM)
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)

lazy val bench = project
  .settings(moduleName := "cats-collections-bench")
  .dependsOn(coreJVM)
  .settings(noPublishSettings)
  .settings(
    buildSettings,
    coverageEnabled := false,
    fork in run := true,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27"
  )
  .enablePlugins(JmhPlugin)

lazy val dogsSettings = buildSettings ++ commonSettings ++ scoverageSettings

lazy val commonSettings =
  compilerFlags ++ Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % V.cats,
      "org.typelevel" %%% "algebra"   % V.algebra,
      compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.9")
    ),
    fork in test := true
  )

addCommandAlias("build", ";compile;test")
addCommandAlias("validate", ";scalastyle;build;docs/tut")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := scalaBinaryVersion.value != "2.11"
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
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

credentials in ThisBuild += Credentials(
  Option(System.getProperty("build.publish.credentials")) map (new File(_)) getOrElse (Path.userHome / ".ivy2" / ".credentials")
)

lazy val publishSettings = Seq(
  publishTo in ThisBuild := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/typelevel/cats-collections")),
  pomIncludeRepository := Function.const(false),
  licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT-"),
                  "BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scmInfo := Some(ScmInfo(url("https://github.com/typeleve/cats-collections"), "scm:git:git@github.com:typelevel/cats-collections.git")),
  autoAPIMappings := true,
  releaseProcess := Nil,
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

lazy val compilerFlags = Seq(
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq(
          "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
          "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:unsound-match",              // Pattern match may not be typesafe.
          "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
          "-Ypartial-unification"              // Enable partial unification in type constructor inference
        )
      case _ =>
        Seq()
    }
  ),
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 11 => // for 2.11 all we care about is capabilities, not warnings
        Seq(
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
        )
      case _ =>
        Seq(
          "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
          "-encoding", "utf-8",                // Specify character encoding used by source files.
          "-explaintypes",                     // Explain type errors in more detail.
          "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
          "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
          "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
          "-Xfuture",                          // Turn on future language features.
          "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
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
          "-Ywarn-dead-code",                  // Warn when dead code is identified.
          "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
          "-Ywarn-numeric-widen",              // Warn when numerics are widened.
          "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals",              // Warn if a local definition is unused.
          "-Ywarn-unused:params",              // Warn if a value parameter is unused.
          "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates",            // Warn if a private member is unused.
          "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
          "-Yrangepos"                         // Syntax highlighting for whole error range.
        )
    }
  ),
  scalacOptions in (Test, compile) --= Seq(
    "-deprecation", // 2.13.0 collections
    "-Xfatal-warnings"
  ),
  scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports"),
  scalacOptions in (Compile, doc)     --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports")
)

