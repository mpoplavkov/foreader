import sbt.Keys.resolvers

name := "foreader"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

lazy val util = project
  .settings(
    libraryDependencies ++= Dependencies.logging ++ Dependencies.cats ++ Dependencies.smile
  )
  .dependsOn(model % "test->test;compile->compile")

lazy val model = project
  .settings(
    libraryDependencies ++= Dependencies.tagging ++ Dependencies.enumeratum ++ Dependencies.test
  )

lazy val core = project
  .settings(
    libraryDependencies ++= Dependencies.stanford ++ Dependencies.json,
    resolvers += Resolver.sonatypeRepo("releases")
  ).dependsOn(model % "test->test;compile->compile", util)

lazy val scripts = project
  .dependsOn(core)
