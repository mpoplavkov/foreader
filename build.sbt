import sbt.Keys.resolvers

name := "foreader"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

lazy val util = project

lazy val model = project
  .settings(
    libraryDependencies ++= Dependencies.tagging ++ Dependencies.enumeratum
  )

lazy val core = project
  .settings(
    libraryDependencies ++= Dependencies.cats ++
      Dependencies.stanford ++
      Dependencies.json ++
      Dependencies.logging ++
      Dependencies.test,
    resolvers += Resolver.sonatypeRepo("releases")
  ).dependsOn(model, util)
