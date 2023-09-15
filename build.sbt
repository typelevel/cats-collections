import com.typesafe.tools.mima.core._

val catsVersion = "2.10.0"
val munitVersion = "1.0.0-M10"
val munitDisciplineVersion = "2.0.0-M3"
val scalacheckVersion = "1.17.0"
val algebraVersion = "2.10.0"
val Scala212 = "2.12.18"
val Scala213 = "2.13.12"
val Scala3 = "3.3.1"

ThisBuild / tlBaseVersion := "0.9"
ThisBuild / startYear := Some(2015)

ThisBuild / crossScalaVersions := Seq(Scala3, Scala213, Scala212)
ThisBuild / tlVersionIntroduced := Map("3" -> "0.9.3")
ThisBuild / tlFatalWarnings := false
ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / tlSitePublishBranch := Some("master")
ThisBuild / githubWorkflowJavaVersions := Seq("8", "11", "17").map(JavaSpec.temurin)
ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "coverage",
    "Coverage",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Use(UseRef.Public("actions", "setup-python", "v2"), name = Some("Setup Python")),
      WorkflowStep.Run(List("pip install codecov"), name = Some("Install Codecov")),
      WorkflowStep
        .Sbt(List("coverage", "testsJVM/test", "testsJVM/coverageReport"), name = Some("Calculate test coverage")),
      WorkflowStep.Run(List("codecov"), name = Some("Upload coverage results"))
    ),
    scalas = List(Scala212),
    javas = List(githubWorkflowJavaVersions.value.head)
  )

lazy val root = tlCrossRootProject.aggregate(core, bench, scalacheck, tests, laws).settings(commonSettings)

lazy val commonJsSettings = Seq(
  coverageEnabled := false,
  tlVersionIntroduced ++= List("2.12", "2.13").map(_ -> "0.9.1").toMap
)

lazy val commonNativeSettings = Seq(
  coverageEnabled := false,
  tlVersionIntroduced ++= List("2.12", "2.13", "3").map(_ -> "0.9.4").toMap
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "cats-collections-core")
  .settings(dogsSettings: _*)
  .settings(
    mimaBinaryIssueFilters ++= {
      if (tlIsScala3.value)
        Seq(
          ProblemFilters.exclude[DirectMissingMethodProblem]("cats.collections.PredicateInstances.$init$")
        )
      else Seq.empty
    }
  )
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "cats-collections-scalacheck")
  .settings(dogsSettings: _*)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion
  )
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(dogsSettings: _*)
  .settings(name := "cats-collections-laws")
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-laws" % catsVersion
  )
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(BuildInfoPlugin, NoPublishPlugin)
  .dependsOn(scalacheck, laws)
  .settings(name := "cats-collections-tests")
  .settings(dogsSettings: _*)
  .settings(
    tlFatalWarnings := false,
    coverageEnabled := false,
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck,
                                         "-minSuccessfulTests",
                                         "1000"
    ), // "-verbosity", "2"), // increase for stress tests
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % catsVersion % Test,
      "org.typelevel" %%% "algebra-laws" % algebraVersion % Test,
      "org.typelevel" %%% "discipline-munit" % munitDisciplineVersion % Test,
      "org.scalameta" %%% "munit" % munitVersion % Test
    ),
    buildInfoPackage := "cats.collections",
    buildInfoKeys := Seq("isJvm" -> (crossProjectPlatform.value == JVMPlatform))
  )
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(dogsSettings: _*)
  .settings(tlFatalWarnings := false)

lazy val bench = project
  .settings(name := "cats-collections-bench")
  .dependsOn(core.jvm)
  .enablePlugins(NoPublishPlugin)
  .settings(
    tlFatalWarnings := false,
    coverageEnabled := false,
    run / fork := true,
    libraryDependencies += {
      val scalazV = if (tlIsScala3.value) "7.4.0-M13" else "7.3.7"
      "org.scalaz" %% "scalaz-core" % scalazV
    }
  )
  .settings(commonSettings)
  .enablePlugins(JmhPlugin)

lazy val dogsSettings = commonSettings ++ scoverageSettings

lazy val commonSettings =
  Seq(
    headerLicense := Some(HeaderLicense.MIT("2015", "Typelevel")),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "algebra" % algebraVersion
    )
  )

addCommandAlias("fmt", "; Compile / scalafmt; Test / scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; Compile / scalafmtCheck; Test / scalafmtCheck; scalafmtSbtCheck")

addCommandAlias("validateJVM", ";testsJVM/compile;testsJVM/test")
addCommandAlias("validateJS", ";testsJS/compile;testsJS/test")

lazy val scoverageSettings = Seq(
  coverageMinimumStmtTotal := 60,
  coverageFailOnMinimum := false
)

ThisBuild / developers += tlGitHubDev("anicolaspp", "Nicolas A Perez")
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"),
                            "BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause")
)
