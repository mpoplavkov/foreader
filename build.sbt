name := "foreader"

version := "0.1"

scalaVersion := "2.12.7"

val SoftwaremillTaggingVersion = "2.2.1"
val CatsEffectVersion = "2.1.1"
val CirceVersion = "0.13.0"

libraryDependencies ++= Seq(
  "com.softwaremill.common" %% "tagging" % SoftwaremillTaggingVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion
)
