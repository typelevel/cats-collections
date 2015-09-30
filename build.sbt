name := "dogs"

organization := "org.typelevel"

scalaVersion in Global := "2.11.7"

crossScalaVersions in Global := Seq("2.10.5", "2.11.7")

resolvers in Global += Resolver.sonatypeRepo("snapshots")

scalacOptions in Global := Seq(
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
  "-Ywarn-unused-import",
  "-Xfuture",
  "-Yno-predef",
  "-Yno-imports")

lazy val dogs = project.in(file(".")).aggregate(core, docs, tests).settings(publish := {})

lazy val core = project

lazy val tests = project dependsOn core

lazy val docs = project dependsOn core

