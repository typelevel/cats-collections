name := "dogs"

organization in Global := "org.typelevel"

scalaVersion in Global := "2.11.7"

crossScalaVersions in Global := Seq("2.10.6", "2.11.7")

resolvers in Global += Resolver.sonatypeRepo("snapshots")

lazy val dogs = project.in(file(".")).aggregate(dogsJVM, dogsJS).settings(publish := {})

lazy val dogsJVM = project.aggregate(coreJVM, docs, testsJVM, bench)
lazy val dogsJS = project.aggregate(coreJS, testsJS)

lazy val core = crossProject.crossType(CrossType.Pure)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val tests = crossProject.crossType(CrossType.Pure) dependsOn core

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project dependsOn coreJVM

lazy val bench = project dependsOn coreJVM

addCommandAlias("buildJVM", ";coreJVM/compile;coreJVM/test;testsJVM/test;bench/test")

addCommandAlias("validateJVM", ";scalastyle;buildJVM;makeSite")

addCommandAlias("validateJS", ";coreJS/compile;testsJS/test")

addCommandAlias("validate", ";validateJS;validateJVM")
