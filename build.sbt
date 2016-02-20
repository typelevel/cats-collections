name := "dogs"

organization in Global := "org.typelevel"

scalaVersion in Global := "2.11.7"

crossScalaVersions in Global := Seq("2.10.5", "2.11.7")

resolvers in Global += Resolver.sonatypeRepo("snapshots")

lazy val dogs = project.in(file(".")).aggregate(core, docs, tests, bench).settings(publish := {})

lazy val core = project

lazy val tests = project dependsOn core

lazy val docs = project dependsOn core

lazy val bench = project dependsOn core
